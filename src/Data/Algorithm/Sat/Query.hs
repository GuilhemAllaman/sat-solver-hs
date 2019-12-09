module Data.Algorithm.Sat.Query
  (
    satisfiable,
    satisfyingAssignment,
    satisfyingAssignments,
    tautology
  ) where

import Data.Maybe
import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Assignment as Assignment
import qualified Data.Algorithm.Sat.Solver as Solver

satisfiable :: (Ord a) => Fml.Fml a -> Bool
satisfiable a = isJust (Solver.solve a)

satisfyingAssignment :: (Ord a) => Fml.Fml a -> Maybe (Assignment.Assignment a)
satisfyingAssignment a = Solver.solve a

satisfyingAssignments :: (Ord a) => Fml.Fml a -> [Assignment.Assignment a]
-- TODO
satisfyingAssignments _ = []

tautology :: (Ord a) => Fml.Fml a -> Bool
-- TODO
tautology _ = False
