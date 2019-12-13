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
preProcess :: Fml.Fml a -> CNFFml.CNFFml a
preProcess = CNFFml.fmlToCNFFml . Fml.toCNF

-- |Solves a formula if possible
-- Returns an Assignment if one has been found, Nothing if not
solve :: (Ord a) => Fml.Fml a -> Maybe (Assignment.Assignment a)
-- TODO
{-
  1. f = preprocess de la formule Fml donnée en entrée
  2. fs = simplification de f
    2a. Si une clause est vide, alors la formule n´est pas solvable -> return NOTHING
    2b. Sinon, on réitère -> etape 2
  3. Si toutes les clauses ont été traitées et la CNFFml est vide ([]), alors la formule est solvable -> Return just Assignment

PS : renvoyer des tuples (CNFFml a, Assignment a) pour gérer l´insertion de variables dans l´assignment
-}
solve _ = Nothing
