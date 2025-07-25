{-# LANGUAGE TupleSections #-}

module Rec where

import Control.Concurrent.STM
import Euterpea
import Euterpea.IO.MIDI.MidiIO
import Types
import Codec.Midi (Channel, Key)
import Control.Monad
import Data.List
import Data.Maybe

-- Play recorded input patterns.
playRec :: TVar Bool -> TVar [(Int, Message)]
        -> ([(Time, MidiMessage)] -> IO ()) -> TVar ChordKeyMap -> Int -> IO()
playRec tRecPlayOn tRecData addMsgsF tChordMap step = do
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
                  . addMsgsF . map ((0.0,) . Std)
                  . concat . mapMaybe applyChordMap'
    registerMsg datStep

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
    f acc (NoteOff ch kEy _) = if (ch, kEy) `elem` onSide acc
                                 then (offSide acc, delete (ch, kEy) $ onSide acc)
                                 else (offSide acc ++ [(ch, kEy)], onSide acc)
    f acc _ = acc
