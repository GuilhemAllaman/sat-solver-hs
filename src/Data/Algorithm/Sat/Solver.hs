module Data.Algorithm.Sat.Solver
  (
    preProcess,
    solve
  ) where

import qualified Data.List as L
import qualified Data.Algorithm.Sat.Utils as Utils
import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Solver.CNFFml as CNFFml
import qualified Data.Algorithm.Sat.Solver.Clause as Clause
import qualified Data.Algorithm.Sat.Assignment as Assignment
import qualified Data.Algorithm.Sat.Lit as Lit


-- |Prepares and cleans a Fml before solving
-- reduces and converts it to CNF format
preProcess :: (Ord a) => Fml.Fml a -> CNFFml.CNFFml a
preProcess = CNFFml.fmlToCNFFml . Fml.toCNF

-- |Solves a formula if possible
-- Returns an Assignment if one has been found, Nothing if not
solve :: (Ord a) => Fml.Fml a -> Maybe (Assignment.Assignment a)
solve f = case (solveRec (preProcess f) Assignment.mkEmpty) of
  Just t -> Just $ snd t
  Nothing -> Nothing

-- |Recursive utilitary function used to solve the formula
-- Passes the up-to-date assignment through the recursion
solveRec :: (Ord a) => CNFFml.CNFFml a -> Assignment.Assignment a -> Maybe (CNFFml.CNFFml a, Assignment.Assignment a)
solveRec f a
  | CNFFml.hasEmptyClause simplified = Nothing
  | length (CNFFml.getClauses simplified) == 0 = Just (simplified, assignment)
  | otherwise = solveRec simplified assignment
    where
      lit = case (CNFFml.findLitToProcess f) of
        Just l -> l
        Nothing -> error("this algorithm is really s*** !!")
      simplified = CNFFml.simplify f lit
      assignment = Assignment.insert lit a
