module Data.Algorithm.Sat.Fml
  (
  Fml(..),
  mkVar,
  --prettyPrinter,
  vars--,
  --toCNF,
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

vars :: Fml a -> [Var.Var a]
vars (Or a b) = vars a ++ vars b
vars (And a b) = vars a ++ vars b
vars (Not a) = vars a
vars (Imply a b) = vars a ++ vars b
vars (Equiv a b) = vars a ++ vars b
vars (XOr a b) = vars a ++ vars b
vars (Final a) = a : []

prettyPrinter :: Fml a -> String
prettyPrinter (Or a b) = "(" ++ prettyPrinter a ++ " | " ++ prettyPrinter b ++ ")"
prettyPrinter (And a b) = "(" ++ prettyPrinter a ++ " & " ++ prettyPrinter b ++ ")"
prettyPrinter (Not a) = "!" ++ prettyPrinter a
prettyPrinter (Imply a b) = "(" ++ prettyPrinter a ++ " => " ++ prettyPrinter b ++ ")"
prettyPrinter (Equiv a b) = "(" ++ prettyPrinter a ++ " <=> " ++ prettyPrinter b ++ ")"
prettyPrinter (XOr a b) = "(" ++ prettyPrinter a ++ " x " ++ prettyPrinter b ++ ")"
prettyPrinter (Final a) = Var.getName a


{-
reduce :: Fml a -> Fml a
reduce (Imply a b) = Or (Not reduce a) (reduce b)
reduce (Equiv a b) = And (Imply (reduce a) (reduce b)) (Imply (reduce b) (reduce a))
reduce (XOr a b) = And (Or (reduce a) (reduce b)) Not(And (reduce a) (reduce b))
reduce a = a
-}

--toCNF :: Fml a -> Fml
