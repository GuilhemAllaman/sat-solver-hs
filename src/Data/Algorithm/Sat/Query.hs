module Data.Algorithm.Sat.Query
  (
    satisfiable,
    satisfyingAssignment,
    satisfyingAssignments,
    tautology
  ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Assignment as Assignment
import qualified Data.Algorithm.Sat.Solver as Solver

satisfiable :: (Ord a) => Fml.Fml a -> Bool
satisfiable f = Maybe.isJust $ Solver.solve f

satisfyingAssignment :: (Ord a) => Fml.Fml a -> Maybe (Assignment.Assignment a)
satisfyingAssignment = Solver.solve

satisfyingAssignments :: (Ord a) => Fml.Fml a -> [Assignment.Assignment a]
-- TODO
satisfyingAssignments _ = []

tautology :: (Ord a) => Fml.Fml a -> Bool
tautology f = case (Solver.solve f) of
  Just a -> M.size (Assignment.getMap a) == 0
  Nothing -> False
