module Data.Algorithm.Sat.Query
  (
    satisfiable,
    satisfyingAssignment,
    satisfyingAssignments,
    tautology
  ) where

import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Assignment as Assignment

satisfiable :: (Ord a) => Fml.Fml a -> Bool
-- TODO
satisfiable _ = True

satisfyingAssignment :: (Ord a) => Fml.Fml a -> Maybe (Assignment.Assignment a)
-- TODO
satisfyingAssignment _ = Nothing

satisfyingAssignments :: (Ord a) => Fml.Fml a -> [Assignment.Assignment a]
-- TODO
satisfyingAssignments _ = []

tautology :: (Ord a) => Fml.Fml a -> Bool
-- TODO
tautology _ = False
