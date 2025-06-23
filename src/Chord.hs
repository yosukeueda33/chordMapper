module Chord (
  ChordType(..), Chord(..), Tension(..), chordTones, getVoicingBetweenOn,
  getEnvelopeDifference, degreeToChord7thOneTension) where

import Euterpea
import Interval
    ( Interval, iPerf, iMaj, iMin, iAug, iDim, intervalPitch )
import Data.Maybe
import Data.List
import Codec.Midi (Key)
import Control.Monad (guard)


data ChordType
  = ChMajor
  | ChMinor
  | ChMinorFlat5
  | ChAug
  | ChSus2
  | ChSus4
  | ChDim
  | Ch7th
  | ChMajor7th
  | ChMinor7th
  | ChMinorMajor7th
  | ChMinor7thFlat5
  | Ch6th
  | ChMinor6th
  | ChDim7th
  | Ch7thSus2
  | Ch7thSus4
  | ChBlackAddor
  deriving (Enum, Bounded, Read, Show)

allChordType :: [ChordType]
allChordType = [minBound .. maxBound]

data Tension
  =  Ts9th
  | TsFlat9th
  | TsSharp9th
  | Ts11th
  | TsSharp11th
  | Ts13th
  | TsFlat13th
  deriving (Enum, Bounded, Read, Show, Ord, Eq)

allTension :: [Tension]
allTension = [minBound .. maxBound]

data Chord = Chord PitchClass ChordType [Tension] deriving (Show)
type ChordPitch = [AbsPitch]

chordTones :: Chord -> Maybe ChordPitch
chordTones (Chord pc chordType tensions) = mapM toPitch
                                  $ intervals chordType ++ map tension tensions
  where
    toPitch :: Interval -> Maybe AbsPitch
    toPitch i = (root +) <$> intervalPitch i 
    root = pcToInt pc
    intervals :: ChordType -> [Interval]
    intervals ChMajor = [iPerf 1, iMaj 3, iPerf 5]
    intervals ChMinor = [iPerf 1, iMin 3, iPerf 5]
    intervals ChMinorFlat5 = [iPerf 1, iMin 3, iDim 5]
    intervals ChAug = [iPerf 1, iMaj 3, iAug 5]
    intervals ChSus2 = [iPerf 1, iMaj 2, iPerf 5]
    intervals ChSus4 = [iPerf 1, iPerf 4, iPerf 5]
    intervals Ch7th = [iPerf 1, iMaj 3, iPerf 5, iMin 7]
    intervals ChMajor7th = [iPerf 1, iMaj 3, iPerf 5, iMaj 7]
    intervals ChMinor7th = [iPerf 1, iMin 3, iPerf 5, iMin 7]
    intervals ChMinorMajor7th = [iPerf 1, iMin 3, iPerf 5, iMaj 7]
    intervals ChMinor7thFlat5 = [iPerf 1, iMin 3, iDim 5, iMin 7]
    intervals Ch6th = [iPerf 1, iMaj 3, iPerf 5, iMaj 6]
    intervals ChMinor6th = [iPerf 1, iMin 3, iPerf 5, iMaj 6]
    intervals ChDim = [iPerf 1, iMin 3, iDim 5]
    intervals ChDim7th = [iPerf 1, iMin 3, iDim 5, iDim 7]
    intervals Ch7thSus2 = [iPerf 1, iMaj 2, iPerf 5, iMin 7]
    intervals Ch7thSus4 = [iPerf 1, iPerf 4, iPerf 5, iMin 7]
    intervals ChBlackAddor = [iPerf 1, iAug 4, iMin 7, iMaj 9]

tension :: Tension -> Interval
tension TsFlat9th = iMin 9 
tension Ts9th = iMaj 9 
tension TsSharp9th = iAug 9 
tension Ts11th = iPerf 11
tension TsSharp11th = iAug 11
tension TsFlat13th = iMin 13
tension Ts13th = iMaj 13



getVoicings :: Int -> Int -> ChordPitch -> [ChordPitch]
getVoicings octRange offsetRange [] = []
getVoicings octRange offsetRange (x:xs)
  = filter (all (\x -> x < 24 && x > -10) ) . addOffset $ (x:) <$> f xs
  where
    f :: ChordPitch -> [ChordPitch]
    f [] = [[]]
    f (x:xs) = do
      oct <- [-octRange .. octRange]
      rs <- f xs
      return $ (oct * 12) + x : rs
    addOffset :: [ChordPitch] -> [ChordPitch]
    addOffset chords = do
      ch <- chords
      offset <- [-offsetRange .. offsetRange]
      return $ ((offset * 12) +) <$> ch 


-- Too heavy.
-- getSmoothInversions :: Int -> Int -> [Chord] -> [ChordPitch]
-- getSmoothInversions offsetRange invRange = head . sortOn getPitchRange
--                                            . getCombis
--                                            . map (getVoicings invRange)
--   where
--     getCombis :: [[ChordPitch]] -> [[ChordPitch]]
--     getCombis [] = [[]]
--     getCombis (cps:cpss) = do
--       cp <- cps
--       offset <- [-offsetRange .. offsetRange]
--       rcps <- getCombis cpss
--       return $ map (+(12 * offset)) cp : rcps


getVoicingBetweenOn :: Ord a => Int -> Int -> (ChordPitch->ChordPitch->a)
                          -> ChordPitch -> ChordPitch -> ChordPitch
getVoicingBetweenOn offsetRange octRange scoreFunc fromChord
  = head . sortOn (scoreFunc fromChord) . getVoicings offsetRange octRange 

getEnvelopeRange :: ChordPitch -> ChordPitch -> Int
getEnvelopeRange c1 c2 = mAx - mIn
  where
    l = c1 ++ c2
    mIn = minimum l
    mAx = maximum l

getEnvelopeDifference :: ChordPitch -> ChordPitch -> Int
getEnvelopeDifference c1 c2 = sum . map abs $ [maximum c1 - maximum c2, minimum c1 - minimum c2]

type Degree = Int

intToPc :: Int -> PitchClass
intToPc i = [C, Df, D, Ef, E, F, Gf, G, Af, A, Bf, B] !! (i `mod` 12)

degreeToChord7th :: PitchClass -> Mode -> Degree -> Chord
degreeToChord7th pc mode deg = Chord root ((types mode) !! ideg) []
  where
    ideg = deg - 1
    root = intToPc $ (getScale pc mode) !! ideg
    types Major = [ ChMajor7th
                  , ChMinor7th
                  , ChMinor7th
                  , ChMajor7th
                  , Ch7th
                  , ChMinor7th
                  , ChMinor7thFlat5
                  ]
    types Minor = [ ChMinor7th
                  , ChMinor7thFlat5
                  , ChMajor7th
                  , ChMinor7th
                  , ChMinor7th
                  , ChMajor7th
                  , Ch7th
                  ]

getScale :: PitchClass -> Mode -> [Key]
getScale rootPc = map (((pcToInt rootPc) +) . fromJust . intervalPitch) . scale
  where
    scale Major = [ iPerf 1
                  , iMaj 2 
                  , iMaj 3 
                  , iPerf 4 
                  , iPerf 5 
                  , iMaj 6 
                  , iMaj 7 
                  ]
    -- Harmonic minor
    scale Minor = [ iPerf 1
                  , iMaj 2 
                  , iMin 3 
                  , iPerf 4 
                  , iPerf 5 
                  , iMin 6 
                  , iMaj 7 
                  ]

findTension :: PitchClass -> Mode -> Chord -> [Tension]
findTension keyPc mode (Chord rootPc typ _) = do
  let ps = fromJust . chordTones $ Chord rootPc typ []
  p <- ps
  t <- allTension
  let i = fromJust . intervalPitch $ tension t
  guard $ elem ((p + i) `mod` 12) (getScale keyPc mode)
  return t

degreeToChord7thOneTension :: PitchClass -> Mode -> Degree -> Chord
degreeToChord7thOneTension pc mode deg = Chord root typ tens
  where
    ch7th@(Chord root typ _) = degreeToChord7th pc mode deg
    tens = [minimum $ findTension pc mode ch7th]

main :: IO ()
main = do
  let two  = Chord D ChMinor7th [Ts9th]
      five = Chord G Ch7th [Ts9th]
      one  = Chord C ChMajor7th [Ts9th]
      six  = Chord A ChMinor7th [Ts9th]
  -- print $ chordTones chord
  -- print $ getVoicings 1 1 . fromJust $ chordTones two
  print $ chordTones two
  print $ chordTones five
  print $ chordTones one
  print $ chordTones six

  let smoothFive = getVoicingBetweenOn 1 1 getEnvelopeDifference
            (fromJust $ chordTones two) (fromJust $ chordTones five)
  print smoothFive
  let smoothOne = getVoicingBetweenOn 1 1 getEnvelopeDifference
            smoothFive (fromJust $ chordTones one)
  print smoothOne
  let smoothSix = getVoicingBetweenOn 1 1 getEnvelopeDifference
            smoothOne (fromJust $ chordTones six)
  print smoothSix

  let smooth2516 = scanl1 (getVoicingBetweenOn 1 1 getEnvelopeDifference)
          $ map (fromJust . chordTones) [two, five, one, six]
  print smooth2516

  let chordsToMusic = line . map (chord . map ((\p -> note wn (p, 100::Volume)) . (+) 60))
  let just2516m = chordsToMusic $ map (fromJust . chordTones) [two, five, one, six]
  let smooth2516m = chordsToMusic smooth2516
  writeMidi "just2516.midi" just2516m
  writeMidi "smooth2516.midi" smooth2516m 
  writeMidi "2516s.midi" $ just2516m :+: smooth2516m

  let chordsToMusic = line . map (chord . map (note en . (+) 60))
  let fives = getVoicings 1 1 . fromJust $ chordTones five
  let ones = getVoicings 1 1 . fromJust $ chordTones one
  writeMidi "G9thInv.midi" $ chordsToMusic fives
  writeMidi "ChMajor9thInv.midi" $ chordsToMusic ones

  let aMinorScale = map (`mod` 12) $ getScale A Minor
  print aMinorScale
  let aMinor2 = degreeToChord7th A Minor 2
  print aMinor2
  let aMinor2t = degreeToChord7thOneTension A Minor 2
  print aMinor2t
  let aMinor251 = map (degreeToChord7thOneTension A Minor) [2, 5, 1]
  print aMinor251
  let chordsToMusic = line . map (chord . map ((\p -> note wn (p, 100::Volume)) . (+) 48))
  let justAMinor251 = chordsToMusic $ map (fromJust . chordTones) aMinor251
  print justAMinor251
  writeMidi "justAMinor251.midi" justAMinor251
  let smoothAMinor251 = chordsToMusic . scanl1 (getVoicingBetweenOn 1 1 getEnvelopeDifference)
          $ map (fromJust . chordTones) aMinor251
  writeMidi "smoothAMinor251.midi" smoothAMinor251