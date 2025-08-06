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

let getDegChord = \(key : Text) -> \(scale : Text) -> \(deg : Natural) ->
      ChordMapConfig.Deg {chordKey = key, chordScale = scale, chordDegree = deg}

let getAbsChord = \(r : Text) -> \(typ : Text) ->
      ChordMapConfig.Abs {chordRoot = r, chordType = typ}

in
{ oneQnSec = 0.6 : Double
, clockOffset = 5 : Natural
, recStepNum = 24*4: Natural -- Shouldn't be changed. It's still buggy.
, isMinilab3 = True
, chordMapSetList = [
      {chordMapSet = [ -- Something about us
        {durationCnf = 24*4, chordCnf = getAbsChord "Bf" "Major7th"}
      , {durationCnf = 24*4, chordCnf = getAbsChord "A" "Minor7th"}
      , {durationCnf = 24*4, chordCnf = getAbsChord "D" "Minor7th"}
      , {durationCnf = 24*4, chordCnf = getAbsChord "G" "7th"}
      ]}
  ]
, specialInputs = [
    {controlType = "NextChordMapSet", messageType="NoteOn", channelNum=9, keyNum=39}
  , {controlType = "RecStart", messageType="NoteOn", channelNum=9, keyNum=42}
  , {controlType = "RecPlayResume", messageType="NoteOn", channelNum=9, keyNum=41}
  , {controlType = "RecPlayStop", messageType="NoteOn", channelNum=9, keyNum=40}
  ]
}
