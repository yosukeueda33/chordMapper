let makeChord = \(dur : Natural)
              -> \(key : Text) -> \(type : Text) -> \(tension : List Text) ->
    {
      chordDuration = dur
    , chordKey = key
    , chordType = type
    , chordTension = tension
    }

in
{ inDevId = 5
, outDevId = 2
, chordMapConfigs = [
                     makeChord (24 * 2) "G" "Minor7th" [ "9th" ]
                   , makeChord (24 * 2) "C" "7th" [ "9th" ]
                   , makeChord (24 * 4) "F" "Major7th" [ "9th" ]
                   ]
-- , chordMapConfigs = [
--                      makeChord (24 * 2) "D" "Minor7th" [ "9th" ]
--                    , makeChord (24 * 2) "G" "7th" [ "9th" ]
--                    , makeChord (24 * 4) "C" "Major7th" [ "9th" ]
--                    ]
}
