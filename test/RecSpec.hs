{-# LANGUAGE TupleSections #-}

module RecSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Euterpea (Message(NoteOff, NoteOn))
import Mapper (getKeyMap)
import Data.Maybe (fromJust)
import Data.List

import Rec

spec :: Spec
spec = do
  describe "wrapProperty" $ do
    it "No unsolved message after wrapping" $ do
      let 
        noteOn = NoteOn <$> choose (0, 15)
                        <*> choose (0, 127)
                        <*> choose (0, 127) ::Gen Message
        noteOff = NoteOff <$> choose (0, 15)
                        <*> choose (0, 127)
                        <*> choose (0, 127) ::Gen Message
        note = oneof [noteOn, noteOff] :: Gen Message
        msgs = listOf note :: Gen [Message]
        stepMsgs = zip
                    <$> listOf (msgs >>= choose . ((0,) . flip (-) 1 <$> length))
                    <*> msgs:: Gen[(Int, Message)]
      property . forAll stepMsgs
        $ \xs -> not (null xs)
            ==> (\(a, b) -> null a && null b)
                . (\wrapped -> filterUnsolvedRecData
                                (0::Int) (length wrapped - 1) wrapped)
                . map snd . wrapUpRecData (length xs) $ sortOn fst xs