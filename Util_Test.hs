module Main where
import Util
import Control.Monad (join)
import Test.QuickCheck.Modifiers (Positive, getPositive)
import Test.QuickCheck.Test (quickCheck)

prop_ChunksOf_Flatten :: Positive Int -> [Char] -> Bool
prop_ChunksOf_Flatten n xs = xs == join (chunksOf (getPositive n) xs)

main = do
  quickCheck prop_ChunksOf_Flatten

