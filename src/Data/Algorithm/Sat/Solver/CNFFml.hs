module Data.Algorithm.Sat.Solver.CNFFml
  (
    CNFFml(..),
    fmlToCNFFml,
    unitaryClause,
    mostCommon,
    litList,
    mostOccurentLit
  ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Function as F
import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Lit as Lit
import qualified Data.Algorithm.Sat.Solver.Clause as Clause

newtype CNFFml a = CNFFml { getClauses :: [Clause.Clause a] }

-- |Converts a Fml formula to the equivalent Lit object
-- The incoming Fml must be in Literal format (Not or Final)
toLit :: Fml.Fml a -> Lit.Lit a
toLit (Fml.Final a) = Lit.T a
toLit (Fml.Not (Fml.Final a)) = Lit.F a

-- |Converts a Fml formula to the equivalent Clause object
-- The incoming Fml must be in Clause format (Or, Not or Final)
toClause :: Fml.Fml a -> Clause.Clause a
toClause (Fml.Or a b) = Clause.Clause (Clause.getLits (toClause a) ++ Clause.getLits (toClause b))
toClause a = Clause.Clause [toLit a]

-- |Converts a Fml formula to the equivalent CNFFml object
-- The incoming Fml is supposed to be in CFN format
fmlToCNFFml :: Fml.Fml a -> CNFFml a
fmlToCNFFml (Fml.And a b) = CNFFml (getClauses (fmlToCNFFml a) ++ getClauses (fmlToCNFFml b))
fmlToCNFFml a = CNFFml [toClause a]

-- |Looks for an unitary clause in the CNFFml's clauses
-- Returns the first unitary clause found, or nothing if no unitary clause found
unitaryClause :: CNFFml a -> Maybe (Clause.Clause a)
unitaryClause = aux . getClauses
  where
    aux [] = Nothing
    aux (x:xs)
      | length (Clause.getLits x) == 1 = Just x
      | otherwise = aux xs

-- |Looks for the most common element of a list
mostCommon :: (Ord a)  => [a] -> Maybe a
mostCommon [] = Nothing
mostCommon a = Just . L.head . L.maximumBy (compare `F.on` L.length) . L.group $ L.sort a

-- |Returns a list of all Lits contained in the CNFFml
litList :: CNFFml a -> [Lit.Lit a]
litList = aux . getClauses
  where
    aux [] = []
    aux a = concat [Clause.getLits x | x <- a]

-- |Looks for the most occurent Lit in all CNFFml's Lits
mostOccurentLit :: (Ord a) =>  CNFFml a -> Maybe (Lit.Lit a)
mostOccurentLit a = mostCommon $ litList a

-- |Chooses the literal to process at each iteration
findLitToProcess :: (Ord a) =>  CNFFml a -> Maybe (Lit.Lit a)
findLitToProcess a = case unitaryClause a of
  Just c -> Just . L.head $ Clause.getLits c
  Nothing -> mostOccurentLit a
