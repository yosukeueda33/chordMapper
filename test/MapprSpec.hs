module MapprSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Mapper (getKeyMap, applyChordMap)
import Data.Maybe (fromJust)
import Data.List
import qualified Data.Map as DMap
import qualified Control.Lens.Internal.Deque as Map
import Euterpea

spec :: Spec
spec = do
  describe "chordMapProperty" $ do
    it "every key input willbe mapped to chord tones." $ do
      let
        normalize = flip mod 12
        inputRange = choose (48, 72)
        chordToneRange = vectorOf 4 $ choose (0, 24)
      property . forAll ((,) <$> chordToneRange <*> inputRange)
               $ \(xs, x) -> all (flip elem (map normalize xs) . normalize)
                           . fromJust $ getKeyMap xs x 

    it "NoteON message that pitch is NOT ON PushingKeyMap should be passed." $ do
      let
        chordKeyMap _ = Just [60]
        pushingKeyMap = DMap.fromList [(20, [61])]
        msg = NoteOn 0 21 0
      applyChordMap chordKeyMap pushingKeyMap msg `shouldBe` [NoteOn 0 60 0]

    it "NoteOff message that pitch is ON PushingKeyMap and SAME input key should be passed." $ do
      let
        chordKeyMap _ = Just [60]
        pushingKeyMap = DMap.fromList [(20, [60])]
        msg = NoteOff 0 20 0
      applyChordMap chordKeyMap pushingKeyMap msg `shouldBe` [NoteOff 0 60 0]

    it "NoteOn message that pitch is already ON PushingKeyMap should be passed. And NoteOff old one." $ do
      let
        chordKeyMap _ = Just [60]
        pushingKeyMap = DMap.fromList [(20, [60])]
        msg = NoteOn 0 21 0
      applyChordMap chordKeyMap pushingKeyMap msg `shouldBe` [NoteOff 0 60 0, NoteOn 0 60 0]

    it "NoteOff message that pitch is already ON PushingKeyMap and DIFFERENT input key should NOT be passed." $ do
      let
        chordKeyMap _ = Just [60]
        pushingKeyMap = DMap.fromList [(20, [60])]
        msg = NoteOff 0 21 0
      applyChordMap chordKeyMap pushingKeyMap msg `shouldBe` []