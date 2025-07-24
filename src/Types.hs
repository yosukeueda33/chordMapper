module Types(PushingKeyMap, ChordKeyMap) where

import qualified Data.Map as Map
import Codec.Midi (Key)

type PushingKeyMap = Map.Map Key [Key]
type ChordKeyMap = (Key -> Maybe [Key])