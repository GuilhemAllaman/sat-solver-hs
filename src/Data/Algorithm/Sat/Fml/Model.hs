module Data.Algorithm.Sat.Fml.Model
  (
    --atLeast,
    anyOf,
    noneOf,
    allOf,
    exactlyOneOf
  ) where

import qualified Data.List as L
import qualified Data.Algorithm.Sat.Fml as Fml

{-atLeast :: (Eq t, Num t, Ord a) => t -> [Fml.Fml a] -> Fml.Fml a
atLeast _ [] = error "Empty list"
atLeast _ [x] = -}

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
