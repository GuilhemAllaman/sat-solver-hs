module Data.Algorithm.Sat.Solver
  (
    solve
  ) where

import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Assignment as Assignment

solve :: (Ord a) => Fml.Fml a -> Maybe (Assignment.Assignment a)
-- TODO
solve _ = Nothing
