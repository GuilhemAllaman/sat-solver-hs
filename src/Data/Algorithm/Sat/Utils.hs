module Data.Algorithm.Sat.Utils
  (
    uniques,
    mostCommon,
    deleteAllInstances,
    combinations
  ) where

import qualified Data.List as L
import qualified Data.Function as F

-- |Removes all duplicates from a List
uniques :: (Ord a) => [a] -> [a]
uniques = L.map L.head . L.group . L.sort

-- |Looks for the most common element of a list
mostCommon :: (Ord a)  => [a] -> Maybe a
mostCommon [] = Nothing
mostCommon a = Just . L.head . L.maximumBy (compare `F.on` L.length) . L.group $ L.sort a

-- |Deletes all instance of an element in a List
deleteAllInstances :: (Eq a) => a -> [a] -> [a]
deleteAllInstances a = L.filter (/=a)

-- |Finds combinations of k elements in a List
combinations :: (Eq a) => Int -> [a] -> [[a]]
combinations k ns = filter ((k==).length) (L.subsequences ns)
