{-# LANGUAGE TupleSections #-}

module Rec(createRecPlay, wrapUpRecData, filterUnsolvedRecData, getMsgToStop) where

import Control.Concurrent.STM
    ( atomically,
      STM,
      newTVarIO,
      readTVar,
      readTVarIO,
      writeTVar,
      TVar )
import Euterpea ( MidiMessage(Std), Message(NoteOff, NoteOn) )
import Euterpea.IO.MIDI.MidiIO ( Time )
import Types ( ChordKeyMap )
import Codec.Midi (Channel, Key)
import Control.Monad ( when )
import Data.List ( delete, sortOn )
import Data.Maybe ( mapMaybe )

createRecPlay :: TVar ChordKeyMap -> Int
              -> IO ( Int -> IO [(Time, MidiMessage)]
                    , Int -> [Message] -> STM ()
                    , Int -> STM [Message]
                    , IO (), IO (), IO ()
                    )
createRecPlay tChordMap lim = do
  tRecPreOn <- newTVarIO False
  tRecOn <- newTVarIO False
  tRecData <- newTVarIO [] :: IO (TVar [(Int, Message)])
  tRecPlayOn <- newTVarIO True
  let
    getRecData' = getRecData tRecPlayOn tRecData tChordMap
    recorder = recordInput lim tRecPreOn tRecOn tRecPlayOn tRecData
    getMsgToStop' step = getMsgToStop step <$> readTVar tRecData <*> readTVar tChordMap
    recStart = atomically $ writeTVar tRecPreOn True
                            >> writeTVar tRecOn False
    recPlayStop =  atomically $ writeTVar tRecPlayOn False
    recPlayResume = atomically $ writeTVar tRecPlayOn True
  return (getRecData', recorder, getMsgToStop', recStart, recPlayResume, recPlayStop)

getMsgToStop :: Int -> [(Int, Message)] -> ChordKeyMap -> [Message]
getMsgToStop step recData cm
  = concat$ mapMaybe (\(ch, key) -> map (($ 0) . NoteOff ch) <$> cm key) unsolved
    where unsolved = snd $ filterUnsolvedRecData 0 step recData :: [(Channel, Key)]

applyChordMap' :: ChordKeyMap -> Message -> Maybe [Message]
applyChordMap' cm (NoteOn chan kEy vol) = map (\x -> NoteOn chan x vol) <$> cm kEy
applyChordMap' cm (NoteOff chan kEy vol) = map (\x -> NoteOff chan x vol) <$> cm kEy
applyChordMap' _ _ = Nothing

-- Play recorded input patterns.
getRecData :: TVar Bool -> TVar [(Int, Message)]
           -> TVar ChordKeyMap
           -> (Int -> IO [(Time, MidiMessage)])
getRecData tRecPlayOn tRecData tChordMap step = do
  on <- readTVarIO tRecPlayOn
  datAll <- readTVarIO tRecData
  cm <- readTVarIO tChordMap
  let
    datStep = map snd $ filter ((==) step . fst) datAll :: [Message]
    cvtMsg :: [Message] -> [(Time, MidiMessage)]
    cvtMsg = map ((0.0,) . Std) . concat . mapMaybe (applyChordMap' cm)
  return $ if on then cvtMsg datStep else []

-- Record input keys to replay looping.
recordInput :: Int -> TVar Bool -> TVar Bool -> TVar Bool
            -> TVar [(Int, Message)]
            -> (Int -> [Message] -> STM ())
recordInput lim tRecPreOn tRecOn tRecPlayOn tRecData step msgs = do
    preOn <- readTVar tRecPreOn
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

type MsgNoVol = (Channel, Key)
type UnsolvedOn = [MsgNoVol]
type UnsolvedOff = [MsgNoVol]

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
    unsolvedOff = fst r
    unsolvedOn = snd r
    r = filterUnsolvedRecData 0 (wrapLen-1) xs

filterUnsolvedRecData :: Int -> Int -> [(Int, Message)] -> (UnsolvedOff, UnsolvedOn)
filterUnsolvedRecData start end
  = foldl accFunc ([], []) . map snd . sortOn fst
    . filter ((\x -> start <= x && x <= end) . fst)
  where
    offSide = fst
    onSide = snd
    accFunc :: ([MsgNoVol], [MsgNoVol]) -> Message -> ([MsgNoVol], [MsgNoVol])
    accFunc acc (NoteOn ch kEy _) = (offSide acc, onSide acc ++ [(ch, kEy)]) 
    accFunc acc (NoteOff ch kEy _) = if (ch, kEy) `elem` onSide acc
                                 then (offSide acc, delete (ch, kEy) $ onSide acc)
                                 else (offSide acc ++ [(ch, kEy)], onSide acc)
    accFunc acc _ = acc