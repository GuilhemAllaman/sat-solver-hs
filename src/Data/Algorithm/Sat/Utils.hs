module Data.Algorithm.Sat.Utils
  (
    mostCommon,
    deleteAllInstance
  ) where

import qualified Data.List as L
import qualified Data.Function as F

-- |Looks for the most common element of a list
mostCommon :: (Ord a)  => [a] -> Maybe a
mostCommon [] = Nothing
mostCommon a = Just . L.head . L.maximumBy (compare `F.on` L.length) . L.group $ L.sort a

-- |Deletes all instance of an element in a List
deleteAllInstance :: (Eq a) => a -> [a] -> [a]
deleteAllInstance a = L.filter (/=a)
