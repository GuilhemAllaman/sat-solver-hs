module Data.Algorithm.Sat.Solver
  (
    preProcess,
    solve,
    simplify,
    simplified
  ) where

import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Var as Var
import qualified Data.Algorithm.Sat.Solver.CNFFml as CNFFml
import qualified Data.Algorithm.Sat.Solver.Clause as Clause
import qualified Data.Algorithm.Sat.Assignment as Assignment
import qualified Data.Algorithm.Sat.Lit as Lit
import qualified Data.List as L

-- |Prepares and cleans a Fml before solving
-- reduces and converts it to CNF format
preProcess :: Fml.Fml a -> CNFFml.CNFFml a
preProcess = CNFFml.fmlToCNFFml . Fml.toCNF . Fml.reduce

-- |Solves a formula if possible
-- Returns an Assignment if one has been found, Nothing if not
solve :: (Ord a) => Fml.Fml a -> Maybe (Assignment.Assignment a)
-- TODO
solve _ = Nothing

deleteAllInstance :: (Eq a) => a -> [a] -> [a]
deleteAllInstance a = L.filter (/=a)

simplify :: (Eq a) => CNFFml.CNFFml a -> Lit.Lit a -> CNFFml.CNFFml a
simplify a b = CNFFml.CNFFml ([Clause.Clause(deleteAllInstance (Lit.neg b) (Clause.getLits x) )| x <- CNFFml.getClauses a, not (b `elem` (Clause.getLits x))])

simplified :: CNFFml.CNFFml Char -> CNFFml.CNFFml Char
simplified a = simplify a (lit (CNFFml.findLitToProcess a))
  where
        lit (Just b) = b
        lit Nothing = Lit.T (Var.mk 'Z')
