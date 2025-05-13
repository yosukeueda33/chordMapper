module Chord where

import Euterpea
import Interval


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
  deriving (Enum, Bounded)

allChordType :: [ChordType]
allChordType = [minBound .. maxBound]

data ChordAdd = ChordAdd ChordType [Tension]

data Tension
  =  Ts9th
  | TsFlat9th
  | TsSharp9th
  | Ts11th
  | TsSharp11th
  | Ts13th
  | TsFlat13th
  deriving (Enum, Bounded)

allTension :: [Tension]
allTension = [minBound .. maxBound]

data Chord = Chord PitchClass ChordType

chordTones :: Chord -> Maybe [AbsPitch] 
chordTones (Chord pc t) = mapM toPitch $ intervals t
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
