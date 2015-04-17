module Main where

import Util

import Control.Monad (join)
import Test.QuickCheck.Modifiers (Positive, getPositive, NonEmptyList, getNonEmpty)
import Test.QuickCheck.Property (Testable)
import Test.QuickCheck.Test (quickCheck)

prop_chunksOf_Flatten :: Positive Int -> [Char] -> Bool
prop_chunksOf_Flatten pn xs = xs == join (chunksOf n xs)
 where
  n = getPositive pn

prop_chunksOf_AllButLastAreN :: Positive Int -> [Char] -> Bool
prop_chunksOf_AllButLastAreN pn xs = all ((==n).length) (dropLast (chunksOf n xs))
 where
  n = getPositive pn
  dropLast [] = []
  dropLast xs = init xs

prop_chunksOf_LastAtMostN :: Positive Int -> NonEmptyList Char -> Bool
prop_chunksOf_LastAtMostN pn xs = (length $ last chunks) <= n
 where
  n = getPositive pn
  chunks = chunksOf n (getNonEmpty xs)

checkProp :: Testable prop => String -> prop -> IO ()
checkProp label prop = do
  putStrLn $ "Testing " ++ label
  quickCheck prop

main = do
  checkProp "prop_chunksOf_Flatten" prop_chunksOf_Flatten
  checkProp "prop_chunksOf_AllButLastAreN" prop_chunksOf_AllButLastAreN
  checkProp "prop_chunksOf_LastAtMostN" prop_chunksOf_LastAtMostN

