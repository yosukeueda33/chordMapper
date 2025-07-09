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

let root = "C"
let scale = "Major"

in
{ oneQnSec = 0.6
, chordMapSetList = [
      {chordMapSet = [
        {durationCnf = 24*4, chordCnf = getDegChord root scale 2}
      , {durationCnf = 24*4, chordCnf = getDegChord root scale 5}
      , {durationCnf = 24*4, chordCnf = getDegChord root scale 1}
      , {durationCnf = 24*4, chordCnf = getDegChord root scale 6}
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
  ]
}
