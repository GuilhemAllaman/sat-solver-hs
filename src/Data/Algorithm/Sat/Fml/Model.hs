module Data.Algorithm.Sat.Fml.Model
  (
  --atLeast,
  --anyOf,
  --noneOf
  ) where

import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.List as List

{-atLeast :: (Eq t, Num t, Ord a) => t -> [Fml.Fml a] -> Fml.Fml a
atLeast _ [] = error "Empty list"-}

--anyOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml

noneOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml a
noneOf = Fml.multAnd . List.map Fml.Not

--allOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml

exactlyOneOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml a
--exactlyOneOf fs =  Fml.multOr [And f (Fml.multAnd (map Not (delete f fs))) | f<-fs]
exactlyOneOf fs = Fml.multOr [aux f | f <- fs]
 where 
  aux f = Fml.And f fs'
   where
    fs' = Fml.multAnd . List.map Fml.Not $ List.delete f fs