{-# LANGUAGE TemplateHaskell #-}
module Main(main) where

import Util

import Test.QuickCheck.Modifiers
  (Positive, getPositive, NonEmptyList, getNonEmpty)
import Test.QuickCheck.All (quickCheckAll)

prop_chunksOf_Empty :: Positive Int -> Bool
prop_chunksOf_Empty pn = chunksOf n ([] :: [Char]) == ([] :: [[Char]])
 where
  n = getPositive pn

prop_chunksOf_Flatten :: Positive Int -> [Char] -> Bool
prop_chunksOf_Flatten pn xs = xs == concat (chunksOf n xs)
 where
  n = getPositive pn

prop_chunksOf_AllButLastAreN :: Positive Int -> NonEmptyList Char -> Bool
prop_chunksOf_AllButLastAreN pn xs = all ((==n).length) (init chunks)
 where
  n = getPositive pn
  chunks = chunksOf n (getNonEmpty xs)

prop_chunksOf_LastAtMostN :: Positive Int -> NonEmptyList Char -> Bool
prop_chunksOf_LastAtMostN pn xs = (length $ last chunks) <= n
 where
  n = getPositive pn
  chunks = chunksOf n (getNonEmpty xs)

-- return [] is TemplateHaskell magic to list the properties.
return []
main = $quickCheckAll

