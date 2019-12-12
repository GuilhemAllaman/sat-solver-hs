module Data.Algorithm.Sat.Fml.Model
  (
    atLeast,
    anyOf,
    noneOf,
    allOf,
    exactlyOneOf
  ) where

import qualified Data.List as L
import qualified Data.Algorithm.Sat.Fml as Fml

atLeast :: (Ord a) => Int -> [Fml.Fml a] -> Fml.Fml a
-- TODO
atLeast n l = Fml.multOr (L.take n l)

anyOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml a
anyOf = Fml.multOr

noneOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml a
noneOf = Fml.multAnd . L.map Fml.Not

allOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml a
allOf = Fml.multAnd

exactlyOneOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml a
--exactlyOneOf fs =  Fml.multOr [And f (Fml.multAnd (map Not (delete f fs))) | f<-fs]
exactlyOneOf fs = Fml.multOr [aux f | f <- fs]
  where
    aux f = Fml.And f (Fml.multAnd . L.map Fml.Not $ L.delete f fs)
