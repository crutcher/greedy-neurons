{-# LANGUAGE TemplateHaskell #-}
module Main(main) where

import Util

import Test.QuickCheck.Modifiers
  (Positive, getPositive, NonEmptyList, getNonEmpty)
import Test.QuickCheck.Property (Testable)
import Test.QuickCheck.All (quickCheckAll)

prop_chunksOf_Flatten :: Positive Int -> [Char] -> Bool
prop_chunksOf_Flatten pn xs = xs == concat (chunksOf n xs)
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

-- return [] is TemplateHaskell magic to list the properties.
return []
main = $quickCheckAll

