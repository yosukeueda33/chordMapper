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

-- let makeChord = \(dur : Natural)
--               -> \(key : Text) -> \(type : Text) -> \(tension : List Text) ->
--     {
--       chordDuration = dur
--     , chordKey = key
--     , chordType = type
--     , chordTension = tension
--     }
let getMinorChord = \(key : Text) -> \(deg : Natural) ->
      ChordMapConfig.Deg {chordKey = key, chordScale = "Minor", chordDegree = deg}

let getChord = \(key : Text) -> \(scale : Text) -> \(deg : Natural) ->
      ChordMapConfig.Deg {chordKey = key, chordScale = scale, chordDegree = deg}

let getAminor = \(deg : Natural) ->
      ChordMapConfig.Deg {chordKey = "A", chordScale = "Minor", chordDegree = deg}

let getCmajor = \(deg : Natural) ->
      ChordMapConfig.Deg {chordKey = "C", chordScale = "Major", chordDegree = deg}

let getFmajor = \(deg : Natural) ->
      ChordMapConfig.Deg {chordKey = "F", chordScale = "Major", chordDegree = deg}

let mode = "Dorian"

in
{ oneQnSec = 0.5
-- , chordMapConfigs = [
--                       {durationCnf = 24*4, chordCnf = getChord "F" "Minor" 2}
--                     , {durationCnf = 24*4, chordCnf = getChord "F" "Minor" 5}
--                     , {durationCnf = 24*8, chordCnf = getChord "F" "Minor" 1}
--                     ] : List {durationCnf : Natural, chordCnf : ChordMapConfig}
, chordMapConfigs = [
                      {durationCnf = 24*4, chordCnf = getChord "A" mode 4}
                    , {durationCnf = 24*4, chordCnf = getChord "A" mode 1}
                    , {durationCnf = 24*4, chordCnf = getChord "A" mode 5}
                    , {durationCnf = 24*4, chordCnf = getChord "A" mode 6}
                    ] : List {durationCnf : Natural, chordCnf : ChordMapConfig}
-- , chordMapConfigs = [
--                       {durationCnf = 24*4, chordCnf = getChord "F" "Minor" 1}
--                     , {durationCnf = 24*4, chordCnf = getChord "F" "Minor" 4}
--                     , {durationCnf = 24*4, chordCnf = getChord "F" "Minor" 3}
--                     , {durationCnf = 24*4, chordCnf = getChord "F" "Minor" 5}
--                     ] : List {durationCnf : Natural, chordCnf : ChordMapConfig}
-- , chordMapConfigs = [
--                       {durationCnf = 24*4, chordCnf = getFmajor 4}
--                     , {durationCnf = 24*4, chordCnf = getFmajor 5}
--                     , {durationCnf = 24*4, chordCnf = getFmajor 3}
--                     , {durationCnf = 24*4, chordCnf = getFmajor 6}
--                     ] : List {durationCnf : Natural, chordCnf : ChordMapConfig}
-- , chordMapConfigs = [
--                       {durationCnf = 24*4, chordCnf = getFmajor 4}
--                     , {durationCnf = 24*4, chordCnf = getFmajor 1}
--                     , {durationCnf = 24*4, chordCnf = getFmajor 5}
--                     , {durationCnf = 24*4, chordCnf = getFmajor 6}
--                     ] : List {durationCnf : Natural, chordCnf : ChordMapConfig}
-- , chordMapConfigs = [ {durationCnf = 24*2, chordCnf = getCmajor 2}
--                     , {durationCnf = 24*2, chordCnf = getCmajor 5}
--                     , {durationCnf = 24*4, chordCnf = getCmajor 1}
--                     ] : List {durationCnf : Natural, chordCnf : ChordMapConfig}
-- , chordMapConfigs = [ {durationCnf = 24*2, chordCnf = getAminor 2}
--                     , {durationCnf = 24*2, chordCnf = getAminor 5}
--                     , {durationCnf = 24*4, chordCnf = getAminor 1}
--                     ] : List {durationCnf : Natural, chordCnf : ChordMapConfig}
-- , chordMapConfigs = [
--                      makeChord (24 * 2) "G" "Minor7th" [ "9th" ]
--                    , makeChord (24 * 2) "C" "7th" [ "9th" ]
--                    , makeChord (24 * 4) "F" "Major7th" [ "9th" ]
--                    ]
-- , chordMapConfigs = [
--                      makeChord (24 * 2) "D" "Minor7th" [ "9th" ]
--                    , makeChord (24 * 2) "G" "7th" [ "9th" ]
--                    , makeChord (24 * 4) "C" "Major7th" [ "9th" ]
--                    ]
}
