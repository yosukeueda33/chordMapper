module Interval (Interval(..), IntervalQuality(..), iPerf, iMaj, iMin, iAug, iDim, intervalPitch) where

import Euterpea
import Control.Monad (guard)

data IntervalQuality = IPerf | IMajor | IMinor | IAug | IDim deriving (Eq, Show)

data Interval = Interval IntervalQuality Int
iPerf = Interval IPerf
iMaj = Interval IMajor
iMin = Interval IMinor
iAug = Interval IAug
iDim = Interval IDim

-- Logical imply
(~>) :: Bool -> Bool -> Bool
(~>) x1 x2 = not x1 || x2

majorInterval :: Int -> Maybe AbsPitch 
majorInterval x = do
  guard $ x > 0
  let
    (oct, n) = divMod (x - 1) 7
    p = [0, 2, 4, 5, 7, 9, 11] !! n
  return $ 12 * oct + p

intervalPitch :: Interval -> Maybe AbsPitch 
intervalPitch (Interval q x) = do
  guard (x > 0)
  let
    n = x `mod` 7
    isPerfect = n `elem` [1, 4, 5]
  guard $ (q == IPerf) ~> isPerfect
  guard $ elem q [IMajor, IMinor] ~> not isPerfect
  let
    diff = case q of
            IPerf -> 0
            IMajor -> 0
            IAug -> 1
            IMinor -> -1
            IDim -> if isPerfect then -1 else -2
  mi <- majorInterval x
  return $ mi + diff 

main :: IO ()
main = do
  let f interval x = show interval ++ " "
                   ++ show x ++ ":"
                   ++ show (intervalPitch $ Interval interval x)
  mapM_ (\i -> mapM_ (putStrLn . f i) [1..16] >> putStrLn "")
        [IPerf, IMajor, IMinor, IAug, IDim]