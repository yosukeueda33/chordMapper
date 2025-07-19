module Chord (
  ChordType(..), Chord(..), Tension(..), chordTones, getVoicingBetweenOn,
  getEnvelopeDifference, degreeToChord7thOneTension,
  degreeToChord7thOnePassingTension, chordTonesTensionAsPassing,
  addNoMin2ndTension ) where

import Euterpea
    ( Mode(..),
      PitchClass(..),
      pcToInt,
      AbsPitch,
      )
import Interval
    ( Interval, iPerf, iMaj, iMin, iAug, iDim, intervalPitch )
import Data.Maybe
import Data.List
import Codec.Midi (Key)
import Control.Monad (guard)
import Data.Bool (bool)


data ChordType
  = ChMajor
  | ChMinor
  | ChMinorFlat5
  | ChAug
  | ChAugMajor7th
  | ChSus2
  | ChSus4
  | ChDim
  | Ch7th
  | Ch7thFlat5
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

-- allChordType :: [ChordType]
-- allChordType = [minBound .. maxBound]

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

intervals :: ChordType -> [Interval]
intervals ChMajor = [iPerf 1, iMaj 3, iPerf 5]
intervals ChMinor = [iPerf 1, iMin 3, iPerf 5]
intervals ChMinorFlat5 = [iPerf 1, iMin 3, iDim 5]
intervals ChAug = [iPerf 1, iMaj 3, iAug 5]
intervals ChAugMajor7th = [iPerf 1, iMaj 3, iAug 5, iMaj 7]
intervals ChSus2 = [iPerf 1, iMaj 2, iPerf 5]
intervals ChSus4 = [iPerf 1, iPerf 4, iPerf 5]
intervals Ch7th = [iPerf 1, iMaj 3, iPerf 5, iMin 7]
intervals Ch7thFlat5 = [iPerf 1, iMaj 3, iDim 5, iMin 7]
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

chordTones :: Chord -> Maybe ChordPitch
chordTones (Chord pc chordType tensions) = mapM toPitch
                                  $ intervals chordType ++ map tension tensions
  where
    toPitch :: Interval -> Maybe AbsPitch
    toPitch i = (root +) <$> intervalPitch i 
    root = pcToInt pc

chordTonesTensionAsPassing :: Chord -> Maybe ChordPitch
chordTonesTensionAsPassing (Chord pc chordType tensions) =
  (++) <$> (mapM toPitch $ intervals chordType)
       <*> ((map (subtract 12)) <$> (mapM (toPitch . tension) tensions))
  where
    toPitch :: Interval -> Maybe AbsPitch
    toPitch i = (root +) <$> intervalPitch i 
    root = pcToInt pc

tension :: Tension -> Interval
tension TsFlat9th = iMin 9 
tension Ts9th = iMaj 9 
tension TsSharp9th = iAug 9 
tension Ts11th = iPerf 11
tension TsSharp11th = iAug 11
tension TsFlat13th = iMin 13
tension Ts13th = iMaj 13



getVoicings :: Int -> Int -> ChordPitch -> [ChordPitch]
getVoicings _ _ [] = []
getVoicings octRange offsetRange (x:xs)
  = filter (all (\x' -> x' < 24 && x' > -10) ) . addOffset $ (x:) <$> f xs
  where
    f :: ChordPitch -> [ChordPitch]
    f [] = [[]]
    f (x'':xs'') = do
      oct <- [-octRange .. octRange]
      rs <- f xs''
      return $ (oct * 12) + x'' : rs
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

getEnvelopeDifference :: ChordPitch -> ChordPitch -> Int
getEnvelopeDifference c1 c2 = sum . map abs $ [maximum c1 - maximum c2, minimum c1 - minimum c2]

type Degree = Int

intToPc :: Int -> PitchClass
intToPc i = [C, Df, D, Ef, E, F, Gf, G, Af, A, Bf, B] !! (i `mod` 12)

rollLeft :: Int -> [a] -> [a]
rollLeft n xs = let l = length xs in take l . drop (n `mod` l) . cycle $ xs

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
    -- Natural minor
    types Minor = [ ChMinor7th
                  , ChMinor7thFlat5
                  , ChMajor7th
                  , ChMinor7th
                  , ChMinor7th
                  , ChMajor7th
                  , Ch7thFlat5
                  ]
    -- -- Harmonic minor
    -- types Minor = [ ChMajor7th
    --               , ChMinor7thFlat5
    --               , ChAugMajor7th
    --               , ChMinor7th
    --               , Ch7th
    --               , ChMajor7th
    --               , ChDim7th
    --               ]
    -- Melodic minor.
    -- types Minor = [ ChMinorMajor7th
    --               , ChMinor7th
    --               , ChAugMajor7th
    --               , Ch7th
    --               , Ch7th
    --               , ChMinor7thFlat5
    --               , ChMinor7thFlat5
    --               ]

    types s =
      let
        modes = [Ionian, Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian]
        shiftNum = snd . head . filter ((==) s . fst) $ zip modes [0..] :: Int
      in
        rollLeft shiftNum $ types Major

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
                  , iMin 7 
                  ]
    -- -- Harmonic minor
    -- scale Minor = [ iPerf 1
    --               , iMaj 2 
    --               , iMin 3 
    --               , iPerf 4 
    --               , iPerf 5 
    --               , iMin 6 
    --               , iMaj 7 
    --               ]
    -- Merodic minor
    -- scale Minor = [ iPerf 1
    --               , iMaj 2 
    --               , iMin 3 
    --               , iPerf 4 
    --               , iPerf 5 
    --               , iMaj 6 
    --               , iMaj 7 
    --               ]
    scale Ionian = scale Major
    scale Dorian = [ iPerf 1
                   , iMaj 2 
                   , iMin 3 
                   , iPerf 4 
                   , iPerf 5 
                   , iMaj 6 
                   , iMin 7 
                   ]
    scale Phrygian = [ iPerf 1
                     , iMin 2 
                     , iMin 3 
                     , iPerf 4 
                     , iPerf 5 
                     , iMin 6 
                     , iMin 7 
                     ]
    scale Lydian = [ iPerf 1
                   , iMaj 2 
                   , iMaj 3 
                   , iAug 4 
                   , iPerf 5 
                   , iMaj 6 
                   , iMaj 7 
                   ]
    scale Mixolydian = [ iPerf 1
                       , iMaj 2 
                       , iMaj 3 
                       , iPerf 4 
                       , iPerf 5 
                       , iMaj 6 
                       , iMin 7 
                       ]
    scale Aeolian = scale Minor
    scale Locrian = [ iPerf 1
                    , iMin 2 
                    , iMin 3 
                    , iPerf 4 
                    , iDim 5 
                    , iMin 6 
                    , iMin 7 
                    ]
    scale _ = []

findTension :: PitchClass -> Mode -> Chord -> Degree -> [Tension]
findTension keyPc mode (Chord rootPc _ _) deg =
  let
    scaledTensions = do
      let p = pcToInt rootPc
      t <- allTension
      let
        i = fromJust . intervalPitch $ tension t
        scaleNotes = map (flip mod 12) (getScale keyPc mode)
      guard $ elem ((p + i) `mod` 12) scaleNotes
      return t
    addAltered ts = if deg == 5
      then
        let
          tgs = [ [Ts9th, TsFlat9th, TsSharp9th]
                , [Ts11th, TsSharp11th]
                , [Ts13th, TsFlat13th]
                ]
        in concatMap (\tg -> bool [] tg $ any (`elem` ts) tg) tgs
      else ts
  in addAltered scaledTensions 

degreeToChord7thOneTension :: PitchClass -> Mode -> Degree -> Chord
degreeToChord7thOneTension pc mode deg = Chord root typ tens
  where
    ch7th@(Chord root typ _) = degreeToChord7th pc mode deg
    tens = [minimum $ findTension pc mode ch7th deg]

findSafePassingTension :: PitchClass -> Mode -> Chord -> Degree -> [Tension]
findSafePassingTension keyPc mode cHord@(Chord rootPc typ _) deg =
  let
    onScaleTension = findTension keyPc mode cHord deg
    findPassing ts = do
                t <- ts
                let i = fromJust . intervalPitch $ tension t
                    passingTone = (pcToInt rootPc) + i - 12
                    ps = fromJust . chordTones $ Chord rootPc typ []
                guard $ all ((> 1) . abs . (passingTone - )) ps
                return t
    onScalePassing = findPassing onScaleTension
    anyPassing = findPassing allTension
  in if null onScalePassing then anyPassing else onScalePassing


degreeToChord7thOnePassingTension :: PitchClass -> Mode -> Degree -> Chord
degreeToChord7thOnePassingTension pc mode deg = Chord root typ tens
  where
    ch7th@(Chord root typ _) = degreeToChord7th pc mode deg
    tens = [minimum $ findSafePassingTension pc mode ch7th deg]

addNoMin2ndTension :: Chord -> Chord
addNoMin2ndTension (Chord rootPc typ _) =
  let
    ps = fromJust . chordTones $ Chord rootPc typ []
    tens = minimum $ do
      t <- allTension
      let i = fromJust . intervalPitch $ tension t
          passingTone = (pcToInt rootPc) + i - 12
      guard $ all ((> 1) . abs . (passingTone - )) ps
      return t
  in Chord rootPc typ [tens]


-- main :: IO ()
-- main = do
--   let chords = map (degreeToChord7thOnePassingTension F Minor) [1,4,3,5]
--   print chords
--   let chordsToMusic = line
--                     . map (chord . map ((\p -> note wn (p, 100::Volume)) . (+) 60))
--   let mus = chordsToMusic
--                     $ map (fromJust . chordTonesTensionAsPassing) chords
--   print mus
--   writeMidi "tmp.midi" mus
--   let smoothed = chordsToMusic . scanl1 (getVoicingBetweenOn 1 1 getEnvelopeDifference)
--           $ map (fromJust . chordTonesTensionAsPassing) chords
--   writeMidi "smoothed.midi" smoothed