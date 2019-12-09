module Data.Algorithm.Sat.Solver.CNFFml
  (
    CNFFml(..),
    fmlToCNFFml,
    unitaryClause,
    litList,
    mostOccurentLit
  ) where

import qualified Data.List as L
import qualified Data.Map as M
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

-- |Returns a list of all Lits contained in the CNFFml
litList :: CNFFml a -> [Lit.Lit a]
-- ATTENTION : dans cette implémentation, les lits ne sont pas uniques, il faudrait les rendre uniques
litList = aux . getClauses
  where
    aux [] = []
    aux a = concat [Clause.getLits x | x <- a]


-- |Creates an occurence map with Lits as keys and occurences as values
litsOccurenceMap :: Ord a => CNFFml a -> M.Map (Lit.Lit a) Int
-- TODO
litsOccurenceMap a = M.fromList $ L.map (\l -> (l, 0)) (litList a)

-- |Looks for the most occurent Lit in CNFFml's Lits
mostOccurentLit :: CNFFml a -> Maybe (Lit.Lit a)
-- TODO
mostOccurentLit _ = Nothing
