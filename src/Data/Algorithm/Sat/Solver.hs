module Data.Algorithm.Sat.Solver
  (
    preProcess,
    solve
  ) where

import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Solver.CNFFml as CNFFml
import qualified Data.Algorithm.Sat.Assignment as Assignment

-- |Prepares and cleans a Fml before solving
-- reduces and converts it to CNF format
preProcess :: Fml.Fml a -> CNFFml.CNFFml a
preProcess = CNFFml.fmlToCNFFml . Fml.toCNF . Fml.reduce

-- |Solves a formula if possible
-- Returns an Assignment if one has been found, Nothing if not
solve :: (Ord a) => Fml.Fml a -> Maybe (Assignment.Assignment a)
-- TODO
solve _ = Nothing
