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
import System.Random
import System.Exit 
import qualified Data.Set as S (Set, insert, empty, delete, null)
import Codec.Midi (Key)
import qualified Data.Map as Map

import Chord
import Chord (ChordType(ChMajor7th, ChMinor7th, Ch7th))

main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  hFlush stdout
--  args <- getArgs
--  let inDev = read (args !! 0)
--      outDev = read (args !! 1)
  getAllDevices >>= print
  let inDev = 5
      outDev = 2
  mainLoop inDev outDev

mainLoop :: Int -> Int -> IO ()
mainLoop inDev outDev = do
    inBuf <- newTVarIO [] -- MIDI buffer for user input 
    genBuf <- newTVarIO [] -- MIDI buffer for generated values
    pushingKeys <- newTVarIO S.empty -- :: TVar [AbsPitch]
    chordMapPre <- newTVarIO Map.empty
    chordMap <- newTVarIO Map.empty
    stopSig <- newTVarIO False -- stop signal
    putStrLn ("Clearing MIDI Devices...")
    wait 0.5
    handleCtrlC stopSig $ do
      putStrLn ("Initializing MIDI Devices...")
      initializeMidi
      forkIO (midiInRec (unsafeInputID inDev) inBuf pushingKeys chordMap stopSig) -- poll input and add to buffer
      forkIO (midiOutRec 0 (unsafeOutputID outDev) inBuf genBuf stopSig) -- take from buffer and output
      -- forkIO (printPushingKeysLoop pushingKeys stopSig)
      forkIO (chordMapPreLoop 0 chordMapPre stopSig)
      forkIO (chordMapLoop pushingKeys chordMapPre chordMap stopSig)
      forkIO (genRec genBuf stopSig) -- a generative placeholder
      putStrLn ("MIDI I/O services started.")
      detectExitLoop stopSig -- should only exit this via handleCtrlC
      terminateMidi -- in case the recursion ends by some irregular means
      exitFailure -- in case the recursion ends by some irregular means
        where handleCtrlC stopSig op = onException op f where
                f = do
                    atomically $ writeTVar stopSig True -- signal the other threads to stop
                    putStrLn "Stopping MIDI devices" -- not clear why Ctrl+C is needed again
                    wait 1.0 -- give the other threads time to stop before closing down MIDI!
                    terminateMidi -- close down MIDI
                    wait 0.5 -- give MIDI time to close down
                    putStrLn "Done. Bye!"
                    exitSuccess 

chordMaps :: [(Seconds, Map.Map Key [Key])]
-- chordMaps = [ (4.0, Map.fromList [(48, [60, 64, 67])]),
--               (4.0, Map.fromList [(48, [59, 62, 67])])]
chordMaps = [ (4.0, getKeyMap two)
            , (4.0, getKeyMap five)
            , (4.0, getKeyMap one)
            , (4.0, getKeyMap six)
            ]
            where
              getKeyMap :: [Key] -> Map.Map Key [Key]
              getKeyMap keys = Map.fromList l
                where l = (48, map (48+) keys)
                        : zipWith (\rk k -> (rk, [k+60])) rightKeys keys
                      rightKeys = [60, 62, 64, 65]
              f = fromJust . chordTones
              two  = f $ Chord D ChMinor7th
              five = f $ Chord G Ch7th
              one  = f $ Chord C ChMajor7th
              six  = f $ Chord A ChMinor7th


chordMapPreLoop :: Int -> TVar (Map.Map Key [Key]) -> TVar Bool -> IO ()
chordMapPreLoop i chordMapPre stopSignal = do
    -- wait 0.01
    stopNow <- readTVarIO stopSignal
    if stopNow then return () else do
      let (duration, m) = chordMaps !! i
      atomically $ writeTVar chordMapPre m 
      print "changed pre"
      wait duration
      -- wait 0.2
      -- print ("1")
      -- wait 0.5
      -- print ("*")
      -- wait 0.5
      -- print (" 2")
      -- wait 0.5
      -- print ("*")
      -- wait 0.5
      -- print ("  3")
      -- wait 0.5
      -- print ("*")
      -- wait 0.5
      -- print ("   4")
      -- wait 0.5
      -- print ("*")
      -- -- wait 0.3
      -- wait 0.5
      -- print $ "changed chord map pre" ++ show m
      chordMapPreLoop ((i + 1) `mod` length chordMaps) chordMapPre stopSignal

chordMapLoop :: TVar (S.Set Key) ->  TVar (Map.Map Key [Key]) -> TVar (Map.Map Key [Key]) -> TVar Bool -> IO ()
chordMapLoop pushingKeys chordMapPre chordMap stopSig = do
    wait 0.01
    stopNow <- readTVarIO stopSig
    if stopNow then return () else do
      pk <- readTVarIO pushingKeys
      if not (S.null pk) then return () else do
        atomically $ do
          cm <- readTVar chordMapPre
          writeTVar chordMap cm
          -- lift print $ "changed chord map" ++ show cm
      chordMapLoop pushingKeys chordMapPre chordMap stopSig

-- Some music that we'll repeat. Think of this as a placeholder for a backing 
-- track or some other computer accompaniment.

nKick = \dura -> note dura (60::AbsPitch)
nTum = \dura -> note dura (67::AbsPitch)
nHighHat = \dura -> note dura (65::AbsPitch)
-- nvHighHat = \dura vol -> note dura (64::AbsPitch, vol::Volume)
nClap = \dura -> note dura (62::AbsPitch)
-- nSp1 = \dura -> note dura (67::AbsPitch, 100::Volume)
-- nSp2 = \dura -> note dura (69::AbsPitch, 100::Volume)

someMusic :: Music AbsPitch
-- someMusic = instrument Percussion $ line $ map (note qn) [60,64,67,64]
someMusic = instrument Percussion
              $ times 2 $ line [nKick qn, rest qn, rest qn, rest qn]
                :=: times 16 (nHighHat en)
                :=: line [nClap dqn, nClap en, rest qn, rest qn, nClap qn, rest qn, rest hn] 
          -- $ chord [
          --           line [nKick qn, rest qn, rest qn, rest qn]
          --         -- , nHighHat en
          --         ]

-- Now we send this to the MIDI out indefinitely, but only adding to the 
-- buffer on an as-needed basis. This is necessary if we were to have a computer 
-- response that adapts to the user's input in some way over time.

myParams =
  PlayParams
    False (predefinedCP customChannelMap) Nothing -- (Just devId)
    1.0 perf
    where
      customChannelMap = [(AcousticGrandPiano, 0), (Percussion, 1), (SynthBass1, 2)]
      -- devId = unsafeOutputID 2
      -- perf = map (\e -> e{eDur = max 0 (eDur e - 0.000001)}) . perform1
      perf = perform1

genRec genBuf stopSig = do
    wait 0.05 -- we're generating a measure at a time; don't need to regen very often
    stopNow <- readTVarIO stopSig
    if stopNow then return () else do
        buf <- readTVarIO genBuf -- what's left in the buffer?
        -- let newMidiMsgs = musicToMsgs' defParams $ someMusic
        let newMidiMsgs = musicToMsgs' myParams someMusic
        if bufAmtGT buf 0.5 then return () else do -- if low buffer, add to it
            putStrLn "Adding music to buffer."
            atomically $ writeTVar genBuf (buf ++ newMidiMsgs)
    genRec genBuf stopSig where
    bufAmtGT :: [(Time,a)] -> Time -> Bool
    bufAmtGT [] tAmt = False
    bufAmtGT ((t,x):txs) tAmt = if t>tAmt then True else bufAmtGT txs (tAmt-t)

-- This function is just to delay main until the user has pressed Ctrl+C.

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

-- MIDI input is received by repeatedly polling the MIDI input device. 
-- Importantly, this can't be done too fast. Trying to do it as fast as the 
-- program can possibly execute will actually result in lag.

midiInRec :: InputDeviceID -> TVar [(Time, MidiMessage)] -> TVar (S.Set Key)
          -> TVar (Map.Map Key [Key]) -> TVar Bool -> IO ()
midiInRec inDev inBuf pushingKeys chordMap stopSignal = do
    wait 0.01 -- must throttle! Otherwise we get lag and may overwhelm MIDI devices.
    stopNow <- readTVarIO stopSignal
    if stopNow then return () else do
        msgs <- mapM getMidiInput [inDev] :: IO [Maybe (Time, [Message])]
        m <- readTVarIO chordMap
        let outVal = concatMap g msgs :: [(Time, MidiMessage)]
                      where
                        g Nothing = []
                        g (Just (t,ms)) = map (\m -> (0, Std m)) $ ms >>= applyChordMap m
        updatePushingKeys pushingKeys msgs
        -- if null outVal then return () else print ("User input: "++show outVal)
        if null outVal then return () else atomically $ addMsgs inBuf outVal
        midiInRec inDev inBuf pushingKeys chordMap stopSignal

applyChordMap :: Map.Map Key [Key] -> Message -> [Message]
applyChordMap m (NoteOn ch key vel) = do
  k <- case Map.lookup key m of
        Just ks -> ks
        Nothing -> return key
  return $ NoteOn ch k vel
applyChordMap m (NoteOff ch key vel) = do
  k <- case Map.lookup key m of
        Just ks -> ks
        Nothing -> return key
  return $ NoteOff ch k vel
applyChordMap _ m = return m

-- MIDI output is done by checking the output buffer and sending messages as 
-- needed based on the current time. We use a TVar for this to communicate 
-- information between the input and output threads. 

updatePushingKeys :: TVar (S.Set Key) -> [Maybe (Time, [Message])] -> IO ()
updatePushingKeys pushingKeys = mapM_ f
  where
    f :: Maybe (Time, [Message]) -> IO ()
    f Nothing = return ()
    f (Just (_, msgs)) = mapM_ ff msgs
    ff :: Message -> IO ()
    ff (NoteOn _ key _) = atomically $ do
      x <- readTVar pushingKeys
      let newVal = S.insert key x
      writeTVar pushingKeys newVal 
    ff (NoteOff _ key _) = atomically $ do
      x <- readTVar pushingKeys
      let newVal = S.delete key x
      writeTVar pushingKeys newVal 
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
        midiOutRec newMsgTime outDev inBuf genBuf stopSig where
    posixFix :: NominalDiffTime -> Double
    posixFix x = fromIntegral (round(x * 1000)) / 1000

-- ====== Just utility functions below this point ======

-- Just add values to the end of the buffer:

addMsgs :: TVar [a] -> [a] -> STM ()
addMsgs v [] = return () 
addMsgs v xs = do
    x <- readTVar v
    let newVal = x ++ xs
    writeTVar v newVal 

-- Clear the buffer after reading it:

getClearMsgs :: TVar [a] -> STM [a]
getClearMsgs v = do 
    x <- readTVar v -- what's in the buffer?
    case x of [] -> return [] -- it's empty, so do nothing
              _ -> do -- it has values!
                  writeTVar v [] -- empty the TVar buffer
                  return x -- return the values it had

-- Read and return/clear events that are ready to be sent to a MIDI device.

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

-- Utilities to interface with lower-level MIDI functions

sendMidiOut :: OutputDeviceID -> [(Time, MidiMessage)] -> IO ()
sendMidiOut dev [] = return ()
sendMidiOut dev ms = outputMidi dev >> (mapM_ (\(t,m) -> deliverMidiEvent dev (0, m))) ms

getMidiInput :: InputDeviceID -> IO (Maybe (Time, [Message])) -- Codec.Midi message format
getMidiInput dev = pollMidi dev

-- Thread delay utility:

type Seconds = Double
wait :: Seconds -> IO () 
wait s = threadDelay $ round $ s * 1000000
