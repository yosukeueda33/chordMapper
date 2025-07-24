module MapprSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Mapper (getKeyMap)
import Data.Maybe (fromJust)

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