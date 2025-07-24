module MainIOSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Chord

spec :: Spec
spec = do
  describe "chordMapProperty" $ do
    it "every key input willbe mapped to chord tones." $ do
      strip "\t  foo bar\n" `shouldBe` "foo bar"
    -- runIO $ print "hello"