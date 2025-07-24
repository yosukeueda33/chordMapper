{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Mapper(mapperMain, getKeyMap) where

import Euterpea
    ( Mode(..),
      PitchClass,
      Message(..),
      MidiMessage(Std),
      DeviceInfo(name),
      OutputDeviceID,
      InputDeviceID
    )
import Euterpea.IO.MIDI.MidiIO
    ( pollMidi,
      deliverMidiEvent,
      outputMidi,
      Time,
      terminateMidi,
      initializeMidi,
      getAllDevices )
import System.IO
import Data.List
import Control.Concurrent
import Control.Concurrent.STM
import Data.Time
import Data.Time.Clock.POSIX
import Data.Maybe
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad
import qualified Data.Text as T
import Data.Bifunctor
import Data.Map (toList)
import Data.Word (Word8)
import Control.Monad.State
import Codec.Midi (Channel, Key)
import Dhall hiding (maybe)

import Chord (Chord(..)
             , chordTonesTensionAsPassing
             , getVoicingBetweenOn, getEnvelopeDifference
             , degreeToChord7thOnePassingTension
             , addNoMin2ndTension)
import Ui
import Types
import ArgParse


data AbsoluteChord = AbsoluteChord
  { chordRoot    :: Text
  , chordType    :: Text
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

data ControlType = NextChordMapSet
                 | RecStart | RecPlayResume | RecPlayStop
                 deriving (Show, Read, Eq)

data SpecialInput = SpecialInput
  { controlType :: Text
  , messageType :: Text
  , channelNum  :: Natural 
  , keyNum      :: Natural 
  } deriving (Show, Generic)

data FullConfig = FullConfig
  { oneQnSec        :: Double
  , recStepNum      :: Natural
  , clockOffset     :: Natural
  , isMinilab3      :: Bool
  , chordMapSetList    :: [ChordMapSet]
  , specialInputs   :: [SpecialInput]
  } deriving (Show, Generic)

instance Dhall.Interpret AbsoluteChord
instance Dhall.Interpret DegreeChord
instance Dhall.Interpret ChordMapEntry
instance Dhall.Interpret ChordMapSet
instance Dhall.Interpret SpecialInput
instance Dhall.Interpret FullConfig


mapperMain :: IO ()
mapperMain = do
  -- Read file path.
  (cfgPath, fontPath) <- genPath
  checkFilePath "config" cfgPath
  checkFilePath "font" fontPath

  -- Load config.
  config <- Dhall.input Dhall.auto $ T.pack cfgPath :: IO FullConfig
  print config

  -- Initialize buffers.
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  hFlush stdout

  --Start/Stop loop.
  loopExitSig <- newEmptyMVar
  loopExitDoneSig <- newMVar ()
  devices <- let f = map $ second name
             in bimap f f <$> getAllDevices :: IO ([(InputDeviceID, String)], [(OutputDeviceID, String)])
  -- UI thread.
  let needOutSubDev = isMinilab3 config
  (uiInput, uiUpdator) <- createUiThread devices needOutSubDev $ T.pack fontPath
  initializeMidi
  -- Wait and execute UI input.
  let
    loop :: IO ()
    loop = do
      i <- uiInput
      case i of
        (UiStart inDev outDev mbOutSubDev) -> do
          _ <- tryTakeMVar loopExitDoneSig
          --Start Midi threads.
          _ <- forkIO $ mainLoop loopExitSig loopExitDoneSig
                        uiUpdator config inDev outDev mbOutSubDev
          putStrLn "Loop Started."
          loop -- Keep listening.
        UiStop -> do
          putMVar loopExitSig ()
          takeMVar loopExitDoneSig
          putStrLn "Loop Stop Done."
          putMVar loopExitDoneSig ()
          loop -- Keep listening.
        UiExit -> do
          putMVar loopExitSig ()
          takeMVar loopExitDoneSig
          putStrLn "Loop Exit Done."
          putMVar loopExitDoneSig ()
  loop

  -- Finish.
  terminateMidi

-- Create Midi threads.
mainLoop :: MVar () -> MVar () -> UiUpdator -> FullConfig
            -> InputDeviceID -> OutputDeviceID -> (Maybe OutputDeviceID) -> IO ()
mainLoop exitSig exitDoneSig uiUpdator config inDev outDev mbOutSubDev = do
    inBuf <- newTVarIO [] -- MIDI buffer for user input 
    genBuf <- newTVarIO [] -- MIDI buffer for generated values
    genSubBuf <- newTVarIO []
    tPushingKeys <- newTVarIO Map.empty
    tChordMapSet <- newTVarIO . genChordMap . chordMapSet . flip (!!) 0 $ chordMapSetList config
    tChordMapSetIndex <- newTVarIO 0
    tChordMap <- newTVarIO (const Nothing)
    tControl <- newTVarIO Nothing
    tChordStep <- newTVarIO 0
    tRecPreOn <- newTVarIO False
    tRecOn <- newTVarIO False
    tRecData <- newTVarIO []
    tRecPlayOn <- newTVarIO True
    preStopSig <- newTVarIO False
    stopSig <- newTVarIO False
    putStrLn "Clearing MIDI Devices..."
    wait 0.5
    let
      qnSec = oneQnSec config
      op = do
        putStrLn "Initializing MIDI Devices..."
        _ <- forkIO $ clockLoop qnSec tChordMapSet tChordMap
                      genBuf preStopSig tChordStep uiUpdator
                      (fromIntegral $ clockOffset config)
                      (playRec tRecPlayOn tRecData inBuf tChordMap)
                      (fmap 
                        (\dev -> atomically . addMsgs (if dev == outDev
                                                           then genBuf
                                                           else genSubBuf))
                        mbOutSubDev)
        let
          subInRec :: [Message] -> IO ()
          subInRec msgs = mapM_ ($ msgs) 
            [ mapM_ (specialInput tControl $ specialInputs config)
            , atomically
              . recordInput (fromIntegral $ recStepNum config)
                  tChordStep tRecPreOn tRecOn tRecPlayOn tRecData
            ]
        _ <- forkIO $ midiInRec inDev inBuf subInRec
                        tPushingKeys tChordMap stopSig -- poll input and add to buffer
        _ <- forkIO $ controlReceiver stopSig tControl -- For Special Input
                        [ ( NextChordMapSet
                          ,  putStrLn "Chord map change registered!"
                              >> changeChordMapSet
                                  tChordMapSet tChordMapSetIndex
                                  (chordMapSetList config))
                        , ( RecStart
                          ,  putStrLn "Rec Start!"
                             >> atomically ( do
                                  writeTVar tRecPreOn True
                                  writeTVar tRecOn False))
                        , ( RecPlayResume
                          ,  putStrLn "Rec Play Resume!"
                             >> atomically (writeTVar tRecPlayOn True))
                        , ( RecPlayStop
                          ,  putStrLn "Rec Play Stop!"
                             >> atomically (writeTVar tRecPlayOn False))
                        ] 
        let
          -- These send queued messages in ellapsed time range.
          -- And return sent message quantity.
          sendOut, sendSubOut, mergedSendOut :: Double -> IO Int
          sendOut ellapsed = do
            outVal1 <- atomically $ getClearMsgs inBuf
            outVal2 <- atomically $ getUpdateMsgs genBuf ellapsed
            sendMidiOut outDev (outVal1++outVal2) -- send out to MIDI device
            return $ length outVal2
          sendSubOut ellapsed = case mbOutSubDev of
            Nothing -> return 0
            Just dev -> if dev == outDev then return 0 else do
              vals <- atomically (getUpdateMsgs genSubBuf ellapsed)
              sendMidiOut dev vals
              return (length vals)
          mergedSendOut ellapsed = sum <$> mapM ($ ellapsed) [sendOut, sendSubOut]
        _ <- forkIO (midiOutRec 0.0 mergedSendOut stopSig) -- take from buffer and output
        putStrLn "MIDI I/O services started."
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

-- Play recorded input patterns.
playRec :: TVar Bool -> TVar [(Int, Message)]
        -> TVar [(Time, MidiMessage)] -> TVar ChordKeyMap -> Int -> IO()
playRec tRecPlayOn tRecData inBuf tChordMap step = do
  on <- readTVarIO tRecPlayOn
  when on $ do
    datAll <- readTVarIO tRecData
    cm <- readTVarIO tChordMap
    let
      datStep = map snd $ filter ((==) step . fst) datAll :: [Message]
      applyChordMap' :: Message -> Maybe [Message]
      applyChordMap' (NoteOn chan kEy vol) = map (\x -> NoteOn chan x vol) <$> cm kEy
      applyChordMap' (NoteOff chan kEy vol) = map (\x -> NoteOff chan x vol) <$> cm kEy
      applyChordMap' _ = Nothing
      registerMsg = unless (null datAll)
                  . atomically . addMsgs inBuf . map ((0.0,) . Std)
                  . concat . mapMaybe applyChordMap'
    registerMsg datStep

-- Invoke tasks by special inputs like tapping pads.
controlReceiver :: TVar Bool -> TVar (Maybe ControlType)
                  -> [(ControlType, IO ())] -> IO ()
controlReceiver stopSig tControl cps = do
  stopNow <- readTVarIO stopSig
  unless stopNow $ do
    mbTc <- readTVarIO tControl
    forM_ mbTc $ \tc -> do
      snd . fromJust $ find ((==) tc. fst) cps -- Execute IO () specified by ControlType.
      atomically $ writeTVar tControl Nothing -- Reset Control.
    -- Repeat
    wait 0.01
    controlReceiver stopSig tControl cps 

-- Roll chord map set list.
changeChordMapSet :: TVar [ChordMap] -> TVar Int -> [ChordMapSet] -> IO ()
changeChordMapSet tChordMapSet tChordMapSetIndex cMapSL = atomically $ do
  nowI <- readTVar tChordMapSetIndex
  let nextI = (nowI + 1) `mod` (length cMapSL)
  writeTVar tChordMapSet . genChordMap . chordMapSet $ cMapSL !! nextI
  writeTVar tChordMapSetIndex nextI

-- Main controll of all Step related task.
clockLoop :: Double -> TVar [ChordMap] -> TVar ChordKeyMap -> TVar [(Time, MidiMessage)]
          -> TVar Bool -> TVar Int -> UiUpdator -> Int -> (Int -> IO ())
          -> Maybe ([(Time, MidiMessage)] -> IO ())
          -> IO ()
clockLoop qnSec tChordMapSet tChordMap genBuf preStopSig tChordStep uiUpdator
          clockDelayStepNum playRecF mbAddSubMsgsF =
  let
    sendSingleMessage x = atomically
                        $ addMsgs genBuf
                            [(0.0, Std $ Reserved 0 $ BL.singleton x)] 
    sendStart = sendSingleMessage 0xFA -- realtime-clock start 
    sendClock = sendSingleMessage 0xF8 -- realtime-clock tick
    sendStop  = sendSingleMessage 0xFC -- realtime-clock stop

    sendDelayedClock :: StateT Int IO ()
    sendDelayedClock = do
      nowCount <- get
      if nowCount < clockDelayStepNum then modify (+1)
      else if nowCount == clockDelayStepNum then (liftIO sendStart) >> modify (+1)
      else (liftIO sendClock)

    waitSingleTick :: StateT Int IO ()
    waitSingleTick = do
      stopNow <- liftIO $ readTVarIO preStopSig
      unless stopNow $ do
        liftIO $ wait $ qnSec / 24.0
        sendDelayedClock
      
    playChordMap :: ChordMap -> StateT Int IO ()
    playChordMap cMap = do
      -- Stop control.
      stopNow <- liftIO $ readTVarIO preStopSig
      unless stopNow $ do
        -- Set chord map.
        let (duration, nAme, m) = cMap
        liftIO $ atomically $ writeTVar tChordMap m
        liftIO $ uiUpdator (Just nAme, Nothing, Nothing)
        -- Ticking while chord duration.
        forM_ [0 .. (duration - 1)]
          $ \x -> liftIO (  uiUpdator ( Nothing
                                      , Just ((duration- 1) - x)
                                      , Nothing
                                      )
                         >> atomically (writeTVar tChordStep x)
                         >> playRecF x
                         >> lightProgress duration x
                         )
                  >> waitSingleTick
    lightProgress :: Int -> Int -> IO ()
    lightProgress dur step =
      let
        sendLights :: ([(Time, MidiMessage)] -> IO ()) -> IO ()
        sendLights addSubMsg = addSubMsg msgs
        lightNum = 8
        nowPos = step `div` (dur `div` lightNum)
        makeMidi = Std . Sysex 0 . BL.pack 
        msgs = [ (0.0, makeMidi $ pad_light (if nowPos == 0 then lightNum - 1 else nowPos - 1) False)
               , (0.0, makeMidi $ pad_light nowPos True)]
      in when (step `mod` (dur `div` lightNum) == 0) $ mapM_ sendLights mbAddSubMsgsF

    pad_light :: Int -> Bool -> [Word8]
    pad_light pos on = [ 0xF0
                       , 0x00, 0x20, 0x6B, 0x7F, 0x42
                       , 0x02, 0x02, 0x16
                       ]
                       ++ [iD] ++ col ++ [0xF7]
      where
        iD = fromIntegral $ ((pos `mod` 8) + 4)
        col = if on then [0x00, 0x00, 0x7F] else [0x7F, 0x7F, 0x7F]


    loop :: StateT Int IO ()
    loop = do
      -- Stop control.
      stopNow <- liftIO $ readTVarIO preStopSig
      unless stopNow $ do
        -- Play one chord map set.
        cMaps <- liftIO $ readTVarIO tChordMapSet
        forM_ (zip cMaps $ reverse [0..((length cMaps) - 1)])
          $ \(cm, p) -> liftIO ( uiUpdator (Nothing, Nothing, Just p))
                        >> playChordMap cm
        loop
  in do
    _ <- execStateT loop 0
    sendStop
    uiUpdator (Just "", Just 0, Just 0)
    putStrLn "closed clock"

type ChordMap = (Int, String, ChordKeyMap)

-- Generate chord map functions from chord specifications.
-- It's not "ChordMapEntry -> ChordMap" for smoothing chords.
genChordMap :: [ChordMapEntry] -> [ChordMap]
genChordMap cfgs =
  let
    names = map f cfgs :: [String]
      where
        f (ChordMapEntry dur (Deg (DegreeChord kEy scale deg)))
          = mconcat [ show dur
                    , " "
                    , T.unpack kEy
                    , " "
                    , T.unpack scale
                    , " "
                    , show deg] :: String
        f (ChordMapEntry dur ((Abs (AbsoluteChord root typ))))
          = mconcat [ show dur
                    , " "
                    , T.unpack root
                    , " "
                    , T.unpack typ] :: String

    chords = map (f . chordCnf) cfgs :: [Chord]
      where
        f :: ChordMapConfig -> Chord
        f (Deg (DegreeChord kEy scale deg))
            = degreeToChord7thOnePassingTension key' scale' deg'
          where
            key' = read $ T.unpack kEy :: PitchClass
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
        f (Abs (AbsoluteChord root typ)) = addNoMin2ndTension $ Chord root' typ' []
          where
            root' = read $ T.unpack root
            typ' = read . ("Ch" ++) $ T.unpack typ

    smoothedChords = scanl1 (getVoicingBetweenOn 1 1 getEnvelopeDifference)
                   $ map (fromJust . chordTonesTensionAsPassing) chords
    -- smoothedChords = map (fromJust . chordTonesTensionAsPassing) chords
    mappers = map getKeyMap smoothedChords :: [ChordKeyMap]
    durations = map (fromIntegral . durationCnf) cfgs
  in
    zip3 durations names mappers


type BlockId = Int
type WhiteInBlockId = Int
type BlackInBlockId = Int
-- Generate input-output key mapping function.
-- Trickey. Should elaborate it.
getKeyMap :: [Key] -> ChordKeyMap
getKeyMap chordTones inputKey = whiteKeyResult <|> blackKeyResult
  where
    offset = -12 * 4
    blackKeyResult :: Maybe [Key] -- For first black key of a Block (4 key separated white key range). It's for root key.
    blackKeyResult = bKeyToTones <$> bKeyIds inputKey
    bKeyIds :: Key -> Maybe (BlockId, BlackInBlockId)
    bKeyIds kEy =
      let
        isBlack k = (k `mod` 12) `elem` [1, 3, 6, 8, 10]
        getBlackInBlockId :: BlockId -> Key -> BlackInBlockId
        getBlackInBlockId bId tKey =
          if isBlack tKey
            then if bId == keyToBlockId (tKey-1)
                   then 1 + getBlackInBlockId bId (tKey-2)
                   else 0
            else getBlackInBlockId bId (tKey-1)
        keyToBlockId = fst . wKeyBlockIds . fromJust . keyToWhiteKeyId
      in do
        guard $ isBlack kEy
        let bId = keyToBlockId (kEy-1)  
            bbId = getBlackInBlockId bId (kEy-2)
        return (bId, bbId)
    bKeyToTones :: (BlockId, BlackInBlockId) -> [Key]
    bKeyToTones (bId, bbId)
      | bbId == 0 = [bId * 12 + head chordTones + offset] -- root
      | bbId == 1 = map (bId * 12 + offset +) $ take 3 chordTones  -- triad
      | bbId == 2 = map (bId * 12 + offset +) . take 3 $ tail chordTones  -- rootless upper triad
      | otherwise = []
    wKeyTones = sort $ tail chordTones
    whiteKeyResult :: Maybe [Key] -- For white key.
    whiteKeyResult = wKeyToTones wKeyTones <$> keyToWhiteKeyId inputKey
    wKeyBlockIds:: Key -> (BlockId, WhiteInBlockId)
    wKeyBlockIds wkey = divMod wkey $ length wKeyTones
    wKeyToTones :: [Key] -> Key -> [Key]
    wKeyToTones tones wkey = [(bId * 12) + (tones !! wbId) + offset]
      where (bId, wbId) = wKeyBlockIds wkey

-- Convert key to whiteKey id.
-- White key id is the id of white piano key. Leftest key on 25-key is 28.
-- The output is Nothing when the key is black.
keyToWhiteKeyId :: Key -> Maybe Int 
keyToWhiteKeyId k = (+ (7 * a)) <$> mb -- 48 -> 28, 60 -> 35, 55 -> (4, 4), 60 -> (5, 0)
  where
    (a, b) = divMod k 12
    mb = elemIndex b whiteKeys
    whiteKeys = [0, 2, 4, 5, 7, 9, 11]

-- To keep mainLoop awake.
detectExitLoop :: TVar Bool -> IO ()
detectExitLoop stopSignal = do
    wait 0.25 -- we only need to check for stopping periodically
    stopNow <- readTVarIO stopSignal
    unless stopNow $ detectExitLoop stopSignal 

type MsgNoVol = (Channel, Key)
-- Wrap up keys that starts with NoteOff or ends with NoteON
-- by putting NoteOn on the start or NoteOff on the end.
-- Start-side wrapping is adhoc. It should be read from Pushing list. 
wrapUpRecData :: Int -> [(Int, Message)] -> [(Int, Message)]
wrapUpRecData wrapLen xs = h ++ xs ++ t
  where
    onVol = 100
    offVol = 0
    h = map (\(ch, kEy) -> (0, NoteOn ch kEy onVol)) unsolvedOff
    t = map (\(ch, kEy) -> (wrapLen-1, NoteOff ch kEy offVol)) unsolvedOn
    offSide = fst
    onSide = snd
    unsolvedOff = offSide r
    unsolvedOn = onSide r

    r :: ([MsgNoVol], [MsgNoVol]) -- (Off list, On list)
    r = foldl f ([], []) $ map snd xs

    f :: ([MsgNoVol], [MsgNoVol]) -> Message -> ([MsgNoVol], [MsgNoVol])
    f acc (NoteOn ch kEy _) = (offSide acc, onSide acc ++ [(ch, kEy)]) 
    f acc (NoteOff ch kEy _) = if ((ch, kEy) `elem` onSide acc)
                                 then (offSide acc, delete (ch, kEy) $ onSide acc)
                                 else (offSide acc ++ [(ch, kEy)], onSide acc)
    f acc _ = acc

-- Record input keys to replay looping.
recordInput :: Int -> TVar Int -> TVar Bool -> TVar Bool -> TVar Bool
            -> TVar [(Int, Message)]-> [Message] -> STM ()
recordInput lim tChordStep tRecPreOn tRecOn tRecPlayOn tRecData msgs = do
    preOn <- readTVar tRecPreOn
    step <- readTVar tChordStep
    when (preOn && (step == 0)) $ do
      writeTVar tRecOn True
      writeTVar tRecData []
      writeTVar tRecPreOn False
      writeTVar tRecPlayOn False
    on <- readTVar tRecOn
    when on $ do
      xs <- readTVar tRecData
      let xs' = xs ++ map (step,) msgs
      if step < (lim - 1) then writeTVar tRecData xs'
      else do
        writeTVar tRecOn False
        writeTVar tRecData $ wrapUpRecData lim xs'
        writeTVar tRecPlayOn True

-- Detect message that assigned as special input like touch pads of Minilab3.
-- It sets TVar when special input detected.
specialInput :: TVar (Maybe ControlType) -> [SpecialInput] -> Message -> IO ()
specialInput tControl cfgs msg =
  let
    f :: SpecialInput -> IO ()
    f cfg =
      let

        cfgMsg :: Message
        cfgMsg = case (T.unpack $ messageType cfg) of
          "NoteOn" -> NoteOn
                         (fromIntegral $ channelNum cfg)
                         (fromIntegral $ keyNum cfg)
                         0
          _ -> error "special input config error"
        control = read . T.unpack $ controlType cfg :: ControlType
        isSameNoteOn (NoteOn c1 k1 _) (NoteOn c2 k2 _) = (c1 == c2) && (k1 == k2)
        isSameNoteOn _ _ = False
      in
        when (isSameNoteOn cfgMsg msg) $ do
          putStrLn "got special input."
          print msg
          atomically . writeTVar tControl $ Just control
        
  in mapM_ f cfgs

-- Midi input thread.
midiInRec :: InputDeviceID -> TVar [(Time, MidiMessage)]
          -> ([Message] -> IO ())
          -> TVar PushingKeyMap
          -> TVar ChordKeyMap -> TVar Bool -> IO ()
midiInRec inDev inBuf subInRec tPushingKeys tChordMap stopSignal = do
  wait 0.01 -- must throttle! Otherwise we get lag and may overwhelm MIDI devices.
  stopNow <- readTVarIO stopSignal
  unless stopNow $ do
    jMsgs <- concatMap snd . catMaybes <$> mapM pollMidi [inDev] :: IO [Message]
    -- Special input like sending next chord map...
    subInRec jMsgs
    -- Normal Input.
    atomically $ do
      chordMap <- readTVar tChordMap
      pushingKeys <- readTVar tPushingKeys
      updatePushingKeys tChordMap tPushingKeys jMsgs
      let outVal = concatMap ( map((0.0::Double,) . Std)
                             . applyChordMap chordMap pushingKeys)
                      jMsgs :: [(Time, MidiMessage)]
      unless (null outVal) $ addMsgs inBuf outVal
    midiInRec inDev inBuf subInRec tPushingKeys tChordMap stopSignal

-- Logical imply.
imply :: Bool -> Bool -> Bool
imply x y = not x || y

-- apply ChordMap to message.
-- The output is List because it can be multiple keys for playing chord with one key.
applyChordMap :: ChordKeyMap -> PushingKeyMap -> Message -> [Message]
applyChordMap chordKeyMap _ (NoteOn ch kEy vel) = do
  k <- fromMaybe [] $ chordKeyMap kEy
  return $ NoteOn ch k vel
applyChordMap chordKeyMap pushingKeyMap (NoteOff ch kEy vel) = do
  k <- fromMaybe [] $ Map.lookup kEy pushingKeyMap
  -- Cancel OFF except last one when there multiple ON.
  let pushingCount
        = length . filter (k ==). concatMap snd $ toList pushingKeyMap
  guard $ isJust ( chordKeyMap kEy) `imply` (pushingCount == 1)
  return $ NoteOff ch k vel
applyChordMap _ _ m = return m

-- Record input key and mapped key to track and NoteOff key that on previous ChordMap.
updatePushingKeys :: TVar ChordKeyMap -> TVar PushingKeyMap -> [Message] -> STM ()
updatePushingKeys tChordMap tPushingKeys = mapM_ f
  where
    f :: Message -> STM ()
    f (NoteOn _ kEy _) = do
      m <- readTVar tPushingKeys
      chordMap <- readTVar tChordMap
      let newVal = (\mks -> Map.insert kEy mks m) <$> chordMap kEy 
      mapM_ (writeTVar tPushingKeys) newVal
    f (NoteOff _ kEy _) = do
      m <- readTVar tPushingKeys
      let newVal = Map.delete kEy m
      writeTVar tPushingKeys newVal 
    f _ = return ()

-- Get posix time.
posixFix :: NominalDiffTime -> Double
posixFix x = fromIntegral (round (x * 1000) :: Integer) / 1000

-- Midi output thread. It just calls sendOut in loop.
midiOutRec :: Double -> (Double -> IO Int) -> TVar Bool -> IO ()
midiOutRec lastMsgTime sendOut stopSig = do
    wait 0.002
    stop <- readTVarIO stopSig 
    unless stop $ do
      currT <- getPOSIXTime -- get the current time
      let
        currT' = posixFix currT
        tEllapsed = currT' - lastMsgTime
      sentSize <- sendOut tEllapsed 
      let newMsgTime = if sentSize == 0 then lastMsgTime else  currT'
      midiOutRec newMsgTime sendOut stopSig
      
-- Add message to buffer.
addMsgs :: TVar [a] -> [a] -> STM ()
addMsgs _ [] = return () 
addMsgs v xs = do
    x <- readTVar v
    let newVal = x ++ xs
    writeTVar v newVal 

-- Get all messages from buffer.
getClearMsgs :: TVar [a] -> STM [a]
getClearMsgs v = do 
    x <- readTVar v -- what's in the buffer?
    case x of [] -> return [] -- it's empty, so do nothing
              _ -> do -- it has values!
                  writeTVar v [] -- empty the TVar buffer
                  return x -- return the values it had

-- Get messages in specified time range from buffer.
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

-- Send Midi to specific device.
sendMidiOut :: OutputDeviceID -> [(Time, MidiMessage)] -> IO ()
sendMidiOut _ [] = return ()
sendMidiOut dev ms = outputMidi dev >> mapM_ (deliverMidiEvent dev . (0,) . snd) ms

type Seconds = Double
-- Wait specified seconds.
wait :: Seconds -> IO () 
wait s = threadDelay $ round $ s * 1000000