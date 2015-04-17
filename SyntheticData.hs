-- Generates synthetic clustered datasets
module SyntheticData
  (booleanClusteredDataSet,
   booleanClusterCenters)
  where

import System.Random
import Data.Matrix
import Util (chunksOf)

booleanClusterCenters :: (RandomGen g) => Int -> Int -> g -> [[Double]]
booleanClusterCenters clusters dims g =
  take clusters $ chunksOf dims $ randoms g

booleanClusteredDataSet :: (RandomGen g) => Int -> Int -> Int -> g -> [(Int, [Bool])]
booleanClusteredDataSet clusters dims outputs g = results
 where
  results = zipWith sample (chunksOf dims (randoms g')) clusterChoices
  sample g c = (c, zipWith (>) (clusterCenters!!c) g)
  clusterCenters = booleanClusterCenters clusters dims g''
  clusterChoices = take outputs $ randomRs (0, clusters-1) g'''
  (g', gn) = split g
  (g'', g''') = split gn
