-- Generates synthetic clustered datasets

import System.Random
import Data.Matrix

repeatedSplitAt :: Int -> [a] -> [[a]]
repeatedSplitAt n = f where
  f [] = []
  f xs = start : f rest where (start, rest) = splitAt n xs

booleanClusterCenters :: (RandomGen g) => Int -> Int -> g -> [[Double]]
booleanClusterCenters clusters dims g =
  take clusters $ repeatedSplitAt dims $ randoms g

booleanClusteredDataSet :: (RandomGen g) => Int -> Int -> Int -> g -> [(Int, [Bool])]
booleanClusteredDataSet clusters dims outputs g = results where
 results = zipWith sample (repeatedSplitAt dims (randoms g')) clusterChoices
 sample g c = (c, zipWith (>) (clusterCenters!!c) g)
 clusterCenters = booleanClusterCenters clusters dims g''
 clusterChoices = take outputs $ randomRs (0, clusters-1) g'''
 (g', gn) = split g
 (g'', g''') = split gn
