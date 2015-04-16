module Util (chunksOf) where

-- Yields sequential chunks of size n from a list.
-- If the list is empty, produces no chunks.
-- If the list is non-empty, the last chunk may have [1, n] elements.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = ys : chunksOf n yss
 where
  (ys, yss) = splitAt n xs

