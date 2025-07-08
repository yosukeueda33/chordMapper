let AbsoluteChord = {
  chordRoot : Text,
  chordType : Text
}

let DegreeChord = {
  chordKey : Text,
  chordScale : Text,
  chordDegree : Natural
}

let ChordMapConfig = <Abs : AbsoluteChord | Deg : DegreeChord>

let getChord = \(key : Text) -> \(scale : Text) -> \(deg : Natural) ->
      ChordMapConfig.Deg {chordKey = key, chordScale = scale, chordDegree = deg}

let getAbsChord = \(r : Text) -> \(typ : Text) ->
      ChordMapConfig.Abs {chordRoot = r, chordType = typ}

--   modes = [Ionian, Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian]
-- let mode = "Ionian"
-- let mode = "Dorian"
-- let mode = "Phrygian"
-- let mode = "Lydian"
-- let mode = "Mixolydian"
let mode = "Aeolian"
-- let mode = "Locrian"
-- let mode = "Minor"
-- let mode = "Minor"
-- let root = "G"
let root = "Ef"

in
{ oneQnSec = 0.6
, chordMapSetList = [
      {chordMapSet = [
        {durationCnf = 24*4, chordCnf = getAbsChord "E" "Major7th"}
      , {durationCnf = 24*4, chordCnf = getAbsChord "Fs" "7th"}
      , {durationCnf = 24*4, chordCnf = getAbsChord "Ds" "Minor7th"}
      , {durationCnf = 24*4, chordCnf = getAbsChord "Gs" "Minor7th"}
      ]
      }
  ] : List { chordMapSet : List {durationCnf : Natural, chordCnf : ChordMapConfig} }
-- , chordMapSetList = [
--       {chordMapSet = [
--         {durationCnf = 24*4, chordCnf = getChord root "Major" 1}
--       , {durationCnf = 24*4, chordCnf = getChord root "Major" 3}
--       , {durationCnf = 24*4, chordCnf = getChord root "Major" 4}
--       , {durationCnf = 24*4, chordCnf = getChord root "Minor" 4}
--       ]
--       }
--   ,   {chordMapSet = [
--         {durationCnf = 24*4, chordCnf = getChord root "Lydian" 4}
--       , {durationCnf = 24*4, chordCnf = getChord root "Lydian" 5}
--       , {durationCnf = 24*4, chordCnf = getChord root "Lydian" 3}
--       , {durationCnf = 24*4, chordCnf = getChord root "Lydian" 6}
--       ]
--       }
--   ] : List { chordMapSet : List {durationCnf : Natural, chordCnf : ChordMapConfig} }
, specialInputs = [
    {controlType = "NextChordMapSet", messageType="NoteOn", channelNum=9, keyNum=39}
  ]
}
