module Data.Algorithm.Sat.Fml
  (
  Fml(..),
  mkVar,
  --toCNF,
  vars--,
  --prettyPrinter
  ) where

import qualified Data.Algorithm.Sat.Var as Var

data Fml a =  Or    (Fml a) (Fml a)
            | And   (Fml a) (Fml a)
            | Not   (Fml a)
            | Imply (Fml a) (Fml a)
            | Equiv (Fml a) (Fml a)
            | XOr   (Fml a) (Fml a)
            | Final (Var.Var a)
            deriving (Show, Eq, Ord)

mkVar :: Char -> Var.Var Char
mkVar = Var.mk

--toCNF :: Fml a -> Fml

vars :: Fml a -> [Var.Var a]
vars (Or a b) = vars a ++ vars b
vars (And a b) = vars a ++ vars b
vars (Not a) = vars a
vars (Imply a b) = vars a ++ vars b
vars (Equiv a b) = vars a ++ vars b
vars (XOr a b) = vars a ++ vars b
vars (Final a) = a : []

--prettyPrinter :: Fml a -> String
