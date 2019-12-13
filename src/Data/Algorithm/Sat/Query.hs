module Data.Algorithm.Sat.Query
  (
    satisfiable,
    satisfyingAssignment,
    constrainedVars,
    notConstrainedVars,
    satisfyingAssignments,
    tautology
  ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Var as Var
import qualified Data.Algorithm.Sat.Lit as Lit
import qualified Data.Algorithm.Sat.Assignment as Assignment
import qualified Data.Algorithm.Sat.Solver as Solver

satisfiable :: (Ord a) => Fml.Fml a -> Bool
satisfiable f = Maybe.isJust $ Solver.solve f

satisfyingAssignment :: (Ord a) => Fml.Fml a -> Maybe (Assignment.Assignment a)
satisfyingAssignment = Solver.solve

extractTuples :: [a] -> [b] -> [(a, b)]
extractTuples a b = [(x,y) | x <- a, y <- b]

constrainedVars :: (Ord a) => Assignment.Assignment a -> [Var.Var a]
constrainedVars = M.keys . Assignment.getMap

notConstrainedVars :: (Ord a) => Fml.Fml a -> Assignment.Assignment a -> [Var.Var a]
notConstrainedVars f a = L.filter (\v -> not . Maybe.isJust $ Assignment.lookup v a) (Fml.uniqueVars f)

satisfyingAssignments :: (Ord a) => Fml.Fml a -> [Assignment.Assignment a]
satisfyingAssignments f = case (Solver.solve f) of
  Nothing -> []
  Just a -> L.filter sameSize (aux (notConstrainedVars f a) [a])
    where
      sameSize = \a -> (M.size $ Assignment.getMap a) == (L.length $ Fml.uniqueVars f)
      aux [] acc = acc
      aux(x:xs) acc = aux xs ((withTrue x acc) ++ (withFalse x acc) ++ acc)
        where
          withTrue x acc = L.map (Assignment.insert $ Lit.mkTrue x) acc
          withFalse x acc = L.map (Assignment.insert $ Lit.mkFalse x) acc

tautology :: (Ord a) => Fml.Fml a -> Bool
tautology f = case (Solver.solve f) of
  Nothing -> False
  Just a -> M.size (Assignment.getMap a) == 0
