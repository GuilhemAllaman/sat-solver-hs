module Data.Algorithm.Sat.Solver.CNFFml
  (
    CNFFml(..),
    fmlToCNFFml
  ) where

import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Lit as Lit
import qualified Data.Algorithm.Sat.Solver.Clause as Clause

newtype CNFFml a = CNFFml { getClauses :: [Clause.Clause a] }

toLit :: Fml.Fml a -> Lit.Lit a
toLit (Fml.Final a) = Lit.T a
toLit (Fml.Not (Fml.Final a)) = Lit.F a

toClause :: Fml.Fml a -> Clause.Clause a
toClause (Fml.Or a b) = Clause.Clause (Clause.getLits (toClause a) ++ Clause.getLits (toClause b))
toClause a = Clause.Clause [toLit a]

fmlToCNFFml :: Fml.Fml a -> CNFFml a
fmlToCNFFml (Fml.And a b) = CNFFml (getClauses (fmlToCNFFml a) ++ getClauses (fmlToCNFFml b))
fmlToCNFFml a = CNFFml [toClause a]
