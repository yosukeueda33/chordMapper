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
  describe "recProperty" $ do
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
                                (0::Int) (maximum $ map fst wrapped) wrapped)
                . wrapUpRecData (10 + maximum (map fst xs)) $ sortOn fst xs
    it "Generate NoteOff for Unsolved NoteON untill specified step" $ do
      let
        step = choose (0, 24 * 8)
        noteOn = (\s k -> (s, NoteOn 1 k 100)) <$> choose (0, 24 * 8) <*> choose (48, 72) :: Gen (Int, Message)
      property
        . forAll ((,) <$> step <*> listOf noteOn)
        $ \(st, recData) -> 
          let
            cnvOnOff (NoteOn ch key _) = NoteOff ch key 0
            recDataBefStep = map (cnvOnOff . snd)
                           $ filter ((st >= ) . fst) (recData :: [(Int, Message)]) :: [Message]
            noteOffs = getMsgToStop st (sortOn fst recData) (Just . (:[])) :: [Message]
          in all (`elem` recDataBefStep) noteOffs
    it "Generate NoteOff for Unsolved NoteON untill specified step. one case" $ do
      let r = getMsgToStop 101 (sortOn fst [
                 (166, NoteOn 1 61 100)
                ,(91,  NoteOn 1 62 100)
                ,(0,   NoteOn 1 53 100)
                ,(88,  NoteOn 1 51 100)
              ]) (Just . (:[]))
      -- print r
      r `shouldBe` [ 
                     NoteOff 1 53 0
                   , NoteOff 1 51 0
                   , NoteOff 1 62 0
                   ]
    it "No unsolved message after wrapping. manual." $ do
      let r = wrapUpRecData 100
                [
                  (0, NoteOn  5  104 118)
                , (0, NoteOff 6  90  78)
                , (1, NoteOff 6  21  34)
                , (2, NoteOn  15 89  113)
                , (2, NoteOn  1  40  101)
                , (6, NoteOff 12 30  60)
                ]
      -- print r
      let r' = filterUnsolvedRecData (0::Int) 100 r
      -- print r'
      r' `shouldBe` ([], [])
    it "Generate NoteOff for Unsolved NoteON untill specified step. manual" $ do
      let
        ons = [ (0,  NoteOn 5 104 118)
              , (5,  NoteOn 4 105 118)
              , (10, NoteOn 3 106 118)
              ]
        st = 5
        noteOffs = getMsgToStop st ons (Just . (:[])) :: [Message]
      print noteOffs
      noteOffs `shouldBe` 
                  [ NoteOff 5 104 0
                  , NoteOff 4 105 0
                  ] 
      