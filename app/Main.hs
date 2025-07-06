{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Euterpea
import Euterpea.IO.MIDI.MidiIO
import System.Exit
import System.IO
import Control.Exception
import Data.List
import System.Environment
import Control.Concurrent
import Control.Concurrent.STM
import Data.Time
import Data.Time.Clock.POSIX
import Data.Maybe
import qualified Data.ByteString.Lazy as BL
import System.Random
import System.Exit 
import qualified Data.Set as S (Set, insert, empty, delete, null)
import Codec.Midi (Key)
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad
import Control.Concurrent.MVar
import qualified Data.Text as T
import Data.Bifunctor
import Monomer
import Dhall

import Chord (ChordType(..), Chord(..), Tension(..), chordTones
             , chordTonesTensionAsPassing
             , getVoicingBetweenOn, getEnvelopeDifference
             , degreeToChord7thOneTension, degreeToChord7thOnePassingTension)
import Ui
import Types


data AbsoluteChord = AbsoluteChord
  { chordRoot    :: Text
  , chordType    :: Text
  , chordTension :: [Text]
  } deriving (Show, Generic)

data DegreeChord = DegreeChord
  { chordKey     :: Text
  , chordScale   :: Text
  , chordDegree  :: Natural
  } deriving (Show, Generic)

data ChordMapConfig
  = Abs AbsoluteChord
  | Deg DegreeChord
  deriving (Show, Generic)

instance Interpret ChordMapConfig where
  autoWith _ =
    Dhall.union
      (  ( constructor "Abs" (Abs <$> Dhall.auto))
      <> ( constructor "Deg" (Deg <$> Dhall.auto))
      )

data ChordMapEntry = ChordMapEntry
  { durationCnf :: Natural
  , chordCnf    :: ChordMapConfig
  } deriving (Show, Generic)

data ChordMapSet = ChordMapSet
  {
    chordMapSet :: [ChordMapEntry]
  } deriving (Show, Generic)

data FullConfig = FullConfig
  { oneQnSec        :: Double
  , chordMapSetList    :: [ChordMapSet]
  } deriving (Show, Generic)

instance Dhall.Interpret AbsoluteChord
instance Dhall.Interpret DegreeChord
instance Dhall.Interpret ChordMapEntry
instance Dhall.Interpret ChordMapSet
instance Dhall.Interpret FullConfig

main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  hFlush stdout

  config <- Dhall.input Dhall.auto "./config/config.dhall" :: IO FullConfig
  print config

  --Start/Stop loop.
  mUiInput <- newEmptyMVar
  loopExitSig <- newEmptyMVar
  loopExitDoneSig <- newMVar ()
  tChordName <- newTVarIO ""
  tUiProgress <- newTVarIO 0
  devices <- let f = map $ bimap id name
             in bimap f f <$> getAllDevices 
  _ <- forkIO $ uiMain mUiInput (UiOutput tChordName tUiProgress) devices
  initializeMidi
  let
    loop :: IO ()
    loop = do
      i <- takeMVar mUiInput :: IO UiInput
      case i of
        (UiStart inDev outDev) -> do
          _ <- tryTakeMVar loopExitDoneSig
          _ <- forkIO $ mainLoop loopExitSig loopExitDoneSig tChordName tUiProgress config inDev outDev
          putStrLn "Loop Started."
          loop
        UiStop -> do
          putMVar loopExitSig ()
          takeMVar loopExitDoneSig
          putStrLn "Loop Stop Done."
          putMVar loopExitDoneSig ()
          loop
        UiExit -> do
          putMVar loopExitSig ()
          takeMVar loopExitDoneSig
          putStrLn "Loop Exit Done."
          putMVar loopExitDoneSig ()
  loop
  terminateMidi

mainLoop :: MVar () -> MVar () -> TVar String -> TVar Int -> FullConfig
            -> InputDeviceID -> OutputDeviceID -> IO ()
mainLoop exitSig exitDoneSig tChordName tUiProgress config inDev outDev = do
    inBuf <- newTVarIO [] -- MIDI buffer for user input 
    genBuf <- newTVarIO [] -- MIDI buffer for generated values
    tPushingKeys <- newTVarIO Map.empty
    tChordMapSet <- newTVarIO . genChordMap . chordMapSet . flip (!!) 0 $ chordMapSetList config
    tChordMap <- newTVarIO (const Nothing)
    preStopSig <- newTVarIO False
    stopSig <- newTVarIO False
    putStrLn ("Clearing MIDI Devices...")
    wait 0.5
    let
      qnSec = oneQnSec config
      op = do
        putStrLn ("Initializing MIDI Devices...")
        _ <- forkIO (clockLoop qnSec tChordMapSet tChordMap
                      genBuf preStopSig tChordName tUiProgress)
        _ <- forkIO (midiInRec inDev inBuf tPushingKeys tChordMap stopSig) -- poll input and add to buffer
        _ <- forkIO (midiOutRec 0 outDev inBuf genBuf stopSig) -- take from buffer and output
        putStrLn ("MIDI I/O services started.")
        detectExitLoop stopSig -- should only exit this via handleCtrlC
      closeOp = do
        atomically $ writeTVar preStopSig True -- signal the other threads to stop
        putStrLn "Stopping clock signal" -- not clear why Ctrl+C is needed again
        wait 1.0
        atomically $ writeTVar stopSig True -- signal the other threads to stop
        putStrLn "Stopping MIDI devices" -- not clear why Ctrl+C is needed again
        wait 2.0 -- give the other threads time to stop before closing down MIDI!
        putStrLn "before terminating..."
        wait 0.5 -- give MIDI time to close down
        putMVar exitDoneSig ()
    _ <- forkIO op
    takeMVar exitSig
    putStrLn "Got exit"
    closeOp

clockLoop :: Double -> TVar [ChordMap] -> TVar ChordKeyMap -> TVar [(Time, MidiMessage)]
          -> TVar Bool -> TVar String -> TVar Int -> IO ()
clockLoop qnSec tChordMapSet tChordMap genBuf preStopSig tChordName tUiProgress =
  let
    sendSingleMessage x = atomically
        $ writeTVar genBuf [(0.0, Std $ Reserved 0 $ BL.singleton x)] 
    start = sendSingleMessage 0xFA -- realtime-clock start 
    stop = do 
      sendSingleMessage 0xFC -- realtime-clock stop
      atomically $ do
        writeTVar tChordName ""
        writeTVar tUiProgress 0
      putStrLn "closed clock"

    waitSingleTick sig = do
      stopNow <- readTVarIO sig
      when (not stopNow) $ do
        wait $ qnSec / 24.0
        sendSingleMessage 0xF8 -- realtime-clock tick
      
    playChordMap cMap = do
      -- Stop control.
      stopNow <- readTVarIO preStopSig
      when (not stopNow) $ do
        -- Set chord map.
        let (duration, name, m) = cMap
        atomically $ do
          writeTVar tChordMap m
          writeTVar tChordName name
        -- Ticking while chord duration.
        mapM_ ( (>> waitSingleTick preStopSig)
              . atomically . writeTVar tUiProgress ) $ reverse [0 .. (duration - 1)]

    loop = do
      -- Stop control.
      stopNow <- readTVarIO preStopSig
      when (not stopNow) $ do
        -- Play one chord map set.
        cMaps <- readTVarIO tChordMapSet
        mapM_ playChordMap cMaps
        loop

  in do
    start
    loop
    stop

type ChordMap = (Int, String, ChordKeyMap)

genChordMap :: [ChordMapEntry] -> [ChordMap]
genChordMap cfgs =
  let
    names = map f cfgs :: [String]
      where
        f (ChordMapEntry dur (Deg (DegreeChord key scale deg)))
          = mconcat [ show dur
                    , " "
                    , T.unpack key
                    , " "
                    , T.unpack scale
                    , " "
                    , show deg] :: String
        f cfg = show cfg

    chords = map (f . chordCnf) cfgs :: [Chord]
      where
        f :: ChordMapConfig -> Chord
        f (Deg (DegreeChord key scale deg))
            = degreeToChord7thOnePassingTension key' scale' deg'
          where
            key' = read $ T.unpack key :: PitchClass
            scale' = sToMode $ T.unpack scale :: Mode
            sToMode s = case s of
              "Major" -> Major
              "Minor" -> Minor
              "Ionian" -> Ionian
              "Dorian" -> Dorian
              "Phrygian" -> Phrygian
              "Lydian" -> Lydian
              "Mixolydian" -> Mixolydian
              "Aeolian" -> Aeolian
              "Locrian" -> Locrian
              _ -> error "config error"
            deg' = fromIntegral deg :: Int
        f _ = error "config error"

    smoothedChords = scanl1 (getVoicingBetweenOn 1 1 getEnvelopeDifference)
                   $ map (fromJust . chordTonesTensionAsPassing) chords
    -- smoothedChords = map (fromJust . chordTonesTensionAsPassing) chords
    mappers = map getKeyMap smoothedChords :: [ChordKeyMap]
    durations = (map (fromIntegral . durationCnf) cfgs)
  in
    zip3 durations names mappers


getKeyMap :: [Key] -> ChordKeyMap
getKeyMap chordTones inputKey = rKeyResult <|> wKeyResult
  where
    offset = -12 * 4
    rKeyResult :: Maybe [Key] 
    rKeyResult = asRightOfWkeyBlockStart <|> asLeftOfWkeyBlockStart
      where
        asRightOfWkeyBlockStart = do
          x <- keyToWhiteKeyId (inputKey - 1)
          guard $ x `mod` 4 == 0
          return $ getRootToneOfTheWkey x
        asLeftOfWkeyBlockStart = Nothing -- For the case the start of white key block is B or F.
        getRootToneOfTheWkey :: Int -> [Key] 
        getRootToneOfTheWkey wkey
          = [head chordTones + (wkey `div` (length chordTones - 1)) * 12 + offset]
    wKeyResult :: Maybe [Key] 
    wKeyResult = wKeyToTones (sort $ tail chordTones) <$> keyToWhiteKeyId inputKey -- The tail for rootless voicing.
    isRootKey x = True
    wKeyToTones :: [Key] -> Int -> [Key]
    wKeyToTones tones wkey = [(bNum * 12) + (tones !! x) + offset]
      where
        (bNum, x) = divMod wkey l
        l = length tones
  
keyToWhiteKeyId :: Key -> Maybe Int 
keyToWhiteKeyId k = (+ (7 * a)) <$> mb -- 48 -> 28, 60 -> 35, 55 -> (4, 4), 60 -> (5, 0)
  where
    (a, b) = divMod k 12
    mb = elemIndex b [0, 2, 4, 5, 7, 9, 11]

detectExitLoop stopSignal = do
    wait 0.25 -- we only need to check for stopping periodically
    stopNow <- readTVarIO stopSignal
    if stopNow then return () else detectExitLoop stopSignal 

printPushingKeysLoop pushingKeys stopSignal = do 
    wait 1.0
    stopNow <- readTVarIO stopSignal
    if stopNow then return () else do
      keys <- readTVarIO pushingKeys
      print keys
      printPushingKeysLoop pushingKeys stopSignal

midiInRec :: InputDeviceID -> TVar [(Time, MidiMessage)] -> TVar PushingKeyMap
          -> TVar ChordKeyMap -> TVar Bool -> IO ()
midiInRec inDev inBuf tPushingKeys tChordMap stopSignal = do
    wait 0.01 -- must throttle! Otherwise we get lag and may overwhelm MIDI devices.
    stopNow <- readTVarIO stopSignal
    if stopNow then return () else do
        msgs <- mapM getMidiInput [inDev] :: IO [Maybe (Time, [Message])]
        atomically $ do
          chordMap <- readTVar tChordMap
          pushingKeys <- readTVar tPushingKeys
          let outVal = concatMap g msgs :: [(Time, MidiMessage)]
                        where
                          g Nothing = []
                          g (Just (t,ms)) = map (\m -> (0, Std m)) $ ms >>= applyChordMap chordMap pushingKeys
          updatePushingKeys tChordMap tPushingKeys msgs
          if null outVal then return () else addMsgs inBuf outVal
          -- if null outVal then return () else lift $ print ("User input: "++show outVal)
        midiInRec inDev inBuf tPushingKeys tChordMap stopSignal

applyChordMap :: ChordKeyMap -> PushingKeyMap -> Message -> [Message]
applyChordMap chordKeyMap _ (NoteOn ch key vel) = do
  k <- case chordKeyMap key of
        Just ks -> ks
        Nothing -> return key
  return $ NoteOn ch k vel
applyChordMap _ pushingKeyMap (NoteOff ch key vel) = do
  k <- case Map.lookup key pushingKeyMap of
        Just ks -> ks
        Nothing -> return key
  return $ NoteOff ch k vel
applyChordMap _ _ m = return m

updatePushingKeys :: TVar ChordKeyMap -> TVar PushingKeyMap -> [Maybe (Time, [Message])] -> STM ()
updatePushingKeys tChordMap tPushingKeys = mapM_ f
  where
    f :: Maybe (Time, [Message]) -> STM ()
    f Nothing = return ()
    f (Just (_, msgs)) = mapM_ ff msgs
    ff :: Message -> STM ()
    ff (NoteOn _ key _) = do
      m <- readTVar tPushingKeys
      chordMap <- readTVar tChordMap
      let newVal = (\mks -> Map.insert key mks m) <$> chordMap key 
      mapM_ (writeTVar tPushingKeys) newVal
    ff (NoteOff _ key _) = do
      m <- readTVar tPushingKeys
      let newVal = Map.delete key m
      writeTVar tPushingKeys newVal 
    ff _ = return ()



midiOutRec :: Double -> OutputDeviceID -> TVar [(Time, MidiMessage)] -> TVar [(Time, MidiMessage)] -> TVar Bool -> IO ()
midiOutRec lastMsgTime outDev inBuf genBuf stopSig = do
    wait 0.001 -- must throttle! Otherwise we get lag and may overwhelm MIDI devices.
    currT <- getPOSIXTime -- get the current time
    let currT' = posixFix currT
        tEllapsed = currT' - lastMsgTime
    stopNow <- atomically $ readTVar stopSig
    if stopNow then return () else do
        outVal1 <- atomically $ getClearMsgs inBuf -- fetch user input
        outVal2 <- atomically $ getUpdateMsgs genBuf tEllapsed -- fetch generated music
        let newMsgTime = if null outVal2 then lastMsgTime else  currT'
        sendMidiOut outDev (outVal1++outVal2) -- send out to MIDI device
        midiOutRec newMsgTime outDev inBuf genBuf stopSig
    where
      posixFix :: NominalDiffTime -> Double
      posixFix x = fromIntegral (round(x * 1000)) / 1000

addMsgs :: TVar [a] -> [a] -> STM ()
addMsgs v [] = return () 
addMsgs v xs = do
    x <- readTVar v
    let newVal = x ++ xs
    writeTVar v newVal 

getClearMsgs :: TVar [a] -> STM [a]
getClearMsgs v = do 
    x <- readTVar v -- what's in the buffer?
    case x of [] -> return [] -- it's empty, so do nothing
              _ -> do -- it has values!
                  writeTVar v [] -- empty the TVar buffer
                  return x -- return the values it had

getUpdateMsgs :: TVar [(Time, a)] -> Time -> STM [(Time, a)]
getUpdateMsgs v tElapsed = do
    ms <- readTVar v -- what's in the buffer?
    case ms of
        [] -> return [] -- nothing, so do nothing
        ((t,x):txs) -> -- it has values!
            let simul = takeWhile ((<=0).fst) txs -- check for 0-timestamped messages
            in  if t <= tElapsed then do -- is it time to send something?
                    writeTVar v (drop (length simul) txs) -- yes, update buffer and then...
                    return ((0,x) : simul) -- return what we removed from the buffer
                else do -- not time yet
                    return []

sendMidiOut :: OutputDeviceID -> [(Time, MidiMessage)] -> IO ()
sendMidiOut dev [] = return ()
sendMidiOut dev ms = outputMidi dev >> (mapM_ (\(t,m) -> deliverMidiEvent dev (0, m))) ms

getMidiInput :: InputDeviceID -> IO (Maybe (Time, [Message])) -- Codec.Midi message format
getMidiInput dev = pollMidi dev

type Seconds = Double
wait :: Seconds -> IO () 
wait s = threadDelay $ round $ s * 1000000
