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
, chordMapSetList = [
      {chordMapSet = [
        {durationCnf = 24*4, chordCnf = getDegChord "A" "Minor" 2}
      , {durationCnf = 24*4, chordCnf = getDegChord "A" "Minor" 5}
      , {durationCnf = 24*8, chordCnf = getDegChord "A" "Minor" 1}
      ]}
  ,   {chordMapSet = [
        {durationCnf = 24*4, chordCnf = getDegChord "F" "Mixolydian" 4}
      , {durationCnf = 24*4, chordCnf = getDegChord "F" "Mixolydian" 1}
      , {durationCnf = 24*4, chordCnf = getDegChord "F" "Mixolydian" 5}
      , {durationCnf = 24*4, chordCnf = getDegChord "F" "Mixolydian" 6}
      ]}
  ,   {chordMapSet = [ -- Tasogare Surround
        {durationCnf = 24*4, chordCnf = getAbsChord "E" "Major7th"}
      , {durationCnf = 24*4, chordCnf = getAbsChord "Fs" "7th"}
      , {durationCnf = 24*4, chordCnf = getAbsChord "Ds" "Minor7th"}
      , {durationCnf = 24*4, chordCnf = getAbsChord "Gs" "Minor7th"}
      ]}
  ]
, specialInputs = [
    {controlType = "NextChordMapSet", messageType="NoteOn", channelNum=9, keyNum=39}
  , {controlType = "RecStart", messageType="NoteOn", channelNum=9, keyNum=42}
  , {controlType = "RecPlayResume", messageType="NoteOn", channelNum=9, keyNum=41}
  , {controlType = "RecPlayStop", messageType="NoteOn", channelNum=9, keyNum=40}
  ]
}
