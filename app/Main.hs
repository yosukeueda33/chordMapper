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
import Monomer
import Dhall

import Chord (ChordType(..), Chord(..), Tension(..), chordTones
             , getVoicingBetweenOn, getEnvelopeDifference, degreeToChord7thOneTension)
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

data FullConfig = FullConfig
  { inDevId         :: Natural
  , outDevId        :: Natural
  , oneQnSec        :: Double
  , chordMapConfigs :: [ChordMapEntry]
  } deriving (Show, Generic)

instance Dhall.Interpret AbsoluteChord
instance Dhall.Interpret DegreeChord
instance Dhall.Interpret ChordMapEntry
instance Dhall.Interpret FullConfig

main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  hFlush stdout

  (inDevs, outDevs) <- getAllDevices
  putStrLn "input devices:"
  mapM_ print inDevs
  putStrLn "output devices:"
  mapM_ print outDevs
  config <- Dhall.input Dhall.auto "./config/config.dhall" :: IO FullConfig
  print config
  mainLoop config 

mainLoop :: FullConfig -> IO ()
mainLoop config = do
    inBuf <- newTVarIO [] -- MIDI buffer for user input 
    genBuf <- newTVarIO [] -- MIDI buffer for generated values
    tPushingKeys <- newTVarIO Map.empty
    tChordMap <- newTVarIO (const Nothing)
    tChordName <- newTVarIO ""
    preStopSig <- newTVarIO False
    stopSig <- newTVarIO False
    exitSig <- newEmptyMVar
    putStrLn ("Clearing MIDI Devices...")
    wait 0.5
    let
      inDev = fromIntegral $ inDevId config
      outDev = fromIntegral $ outDevId config
      cMaps = genChordMap $ chordMapConfigs config
      qnSec = oneQnSec config
      op = do
        putStrLn ("Initializing MIDI Devices...")
        initializeMidi
        _ <- forkIO (clockLoop qnSec cMaps tChordMap genBuf preStopSig tChordName)
        _ <- forkIO (midiInRec (unsafeInputID inDev) inBuf tPushingKeys tChordMap stopSig) -- poll input and add to buffer
        _ <- forkIO (midiOutRec 0 (unsafeOutputID outDev) inBuf genBuf stopSig) -- take from buffer and output
        _ <- forkIO $ uiMain exitSig tChordName
        putStrLn ("MIDI I/O services started.")
        detectExitLoop stopSig -- should only exit this via handleCtrlC
        terminateMidi -- in case the recursion ends by some irregular means
      closeOp = do
        atomically $ writeTVar preStopSig True -- signal the other threads to stop
        putStrLn "Stopping clock signal" -- not clear why Ctrl+C is needed again
        wait 1.0
        atomically $ writeTVar stopSig True -- signal the other threads to stop
        putStrLn "Stopping MIDI devices" -- not clear why Ctrl+C is needed again
        wait 2.0 -- give the other threads time to stop before closing down MIDI!
        putStrLn "before terminating..."
        terminateMidi -- close down MIDI
        putStrLn "terminating..."
        wait 0.5 -- give MIDI time to close down
        putStrLn "Done. Bye!"
    _ <- forkIO op
    takeMVar exitSig
    putStrLn "Got exit"
    closeOp

clockLoop :: Double -> [ChordMap] -> TVar ChordKeyMap -> TVar [(Time, MidiMessage)]
          -> TVar Bool -> TVar String -> IO ()
clockLoop qnSec cMaps tChordMap genBuf preStopSig tChordName =
  let
    f x = atomically
        $ writeTVar genBuf [(0.0, Std $ Reserved 0 $ BL.singleton x)] 
    start = f 0xFA 
    stop = do 
      f 0xFC
      putStrLn "closed clock"
    loop iChord iWait = do
      stopNow <- readTVarIO preStopSig
      if stopNow then return () else do
        -- The 24 is realtime-clock count of MIDI.
        wait $ qnSec / 24.0
        f 0xF8
        let
          (duration, name, m) = cMaps !! iChord
        if iWait >= (duration - 1) then do
          let nextIwait  = 0
              nextIchord = (iChord + 1) `mod` length cMaps 
              (duration', name', m') = cMaps !! nextIchord
          atomically $ writeTVar tChordMap m' 
          atomically $ writeTVar tChordName name'
          -- print $ "changed to" ++ name'
          loop nextIchord nextIwait
        else do
          let nextIwait = iWait + 1
          loop iChord nextIwait

  in do
    start
    loop 0 1
    stop

type ChordMap = (Int, String, ChordKeyMap)

genChordMap :: [ChordMapEntry] -> [ChordMap]
genChordMap cfgs =
  let
    names = map f cfgs
      where
        f (ChordMapEntry dur (Deg (DegreeChord key scale deg)))
          = show dur ++ " " ++ show key ++ show scale ++ show deg
        f cfg = show cfg

    chords = map (f . chordCnf) cfgs :: [Chord]
      where
        f :: ChordMapConfig -> Chord
        f (Deg (DegreeChord key scale deg)) = degreeToChord7thOneTension key' scale' deg'
          where
            key' = read $ T.unpack key :: PitchClass
            scale' = sToMode $ T.unpack scale :: Mode
            sToMode s = case s of
              "Major" -> Major
              "Minor" -> Minor
              _ -> error "config error"
            deg' = fromIntegral deg :: Int
        f _ = error "config error"

    smoothedChords = scanl1 (getVoicingBetweenOn 1 1 getEnvelopeDifference)
                   $ map (fromJust . chordTones) chords
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
