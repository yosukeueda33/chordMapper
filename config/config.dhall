let AbsoluteChord = {
  chordRoot : Text,
  chordType : Text,
  chordTension : List Text
}

let DegreeChord = {
  chordKey : Text,
  chordScale : Text,
  chordDegree : Natural
}

let ChordMapConfig = <Abs : AbsoluteChord | Deg : DegreeChord>

let getChord = \(key : Text) -> \(scale : Text) -> \(deg : Natural) ->
      ChordMapConfig.Deg {chordKey = key, chordScale = scale, chordDegree = deg}

-- let mode = "Mixolydian"
let mode = "Dorian"
-- let mode = "Phrygian"
-- let mode = "Minor"
-- let mode = "Minor"
let root = "G"
-- let root = "E"

in
{ oneQnSec = 0.6
, chordMapSetList = [
      {chordMapSet = [
        {durationCnf = 24*4, chordCnf = getChord root "Dorian" 2}
      , {durationCnf = 24*4, chordCnf = getChord root "Dorian" 5}
      , {durationCnf = 24*8, chordCnf = getChord root "Dorian" 1}
      ]
      }
  ,   {chordMapSet = [
        {durationCnf = 24*4, chordCnf = getChord root "Lydian" 4}
      , {durationCnf = 24*4, chordCnf = getChord root "Lydian" 5}
      , {durationCnf = 24*4, chordCnf = getChord root "Lydian" 3}
      , {durationCnf = 24*4, chordCnf = getChord root "Lydian" 6}
      ]
      }
  ] : List { chordMapSet : List {durationCnf : Natural, chordCnf : ChordMapConfig} }
, specialInputs = [
    {controlType = "NextChordMapSet", messageType="NoteOn", channelNum=9, keyNum=39}
  ]
}
