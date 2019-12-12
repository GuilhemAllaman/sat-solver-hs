module Data.Algorithm.Sat.Fml
  (
    Fml(..),
    mkVar,
    prettyPrinter,
    multAnd,
    multOr,
    vars,
    reduce,
    toCNF,
    findClauses
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


mkVar :: a -> Fml a
mkVar a = Final(Var.mk a)


vars :: Fml a -> [Var.Var a]
vars (Or a b) = vars a ++ vars b
vars (And a b) = vars a ++ vars b
vars (Not a) = vars a
vars (Imply a b) = vars a ++ vars b
vars (Equiv a b) = vars a ++ vars b
vars (XOr a b) = vars a ++ vars b
vars (Final a) = a : []


prettyPrinter :: Show a => Fml a -> String
prettyPrinter (Or a b) = "(" ++ prettyPrinter a ++ " | " ++ prettyPrinter b ++ ")"
prettyPrinter (And a b) = "(" ++ prettyPrinter a ++ " & " ++ prettyPrinter b ++ ")"
prettyPrinter (Not a) = "!" ++ prettyPrinter a
prettyPrinter (Imply a b) = "(" ++ prettyPrinter a ++ " => " ++ prettyPrinter b ++ ")"
prettyPrinter (Equiv a b) = "(" ++ prettyPrinter a ++ " <=> " ++ prettyPrinter b ++ ")"
prettyPrinter (XOr a b) = "(" ++ prettyPrinter a ++ " x " ++ prettyPrinter b ++ ")"
prettyPrinter (Final v) = show v


multAnd :: [Fml a] -> Fml a
multAnd [] = error "Empty list"
multAnd (x:[]) = x
multAnd (x:xs) = And x (multAnd xs)


multOr :: [Fml a] -> Fml a
multOr [] = error "Empty list"
multOr (x:[]) = x
multOr (x:xs) = Or x (multAnd xs)


reduce :: Fml a -> Fml a
reduce (Imply a b) = Or (Not $ reduce a) $ reduce b
reduce (Equiv a b) = And (Or (Not $ reduce a) $ reduce b) $ Or (Not $ reduce b) $ reduce a
reduce (XOr a b) = Or (And (reduce a) $ Not (reduce b)) (And (reduce b) $ Not (reduce a))
reduce (Or a b) = Or (reduce a)  (reduce b)
reduce (And a b) = And (reduce a) ( reduce b)
reduce (Not a) = Not $ reduce a
reduce (Final a) = Final a


toCNF :: Fml a -> Fml a
toCNF (Final a) = Final a
toCNF (And a b) = And (toCNF a) (toCNF b)
toCNF (Imply a b) = toCNF $ Or (Not a) b
toCNF (Equiv a b) = toCNF $ Or (And a b) (And (Not a) (Not b))
toCNF (XOr a b) = toCNF $ Or (And a (Not b)) (And (Not a) b)
toCNF (Or a b) = multAnd [Or p q | p <- aux a, q <- aux b]
  where
    aux = findClauses . toCNF
toCNF (Not a) = aux a
  where
    aux (Final a) = Not (Final a)
    aux (Not a) = toCNF a
    aux (And a b) = toCNF $ Or (Not a) (Not b)
    aux (Or a b) = toCNF $ And (Not a) (Not b)
    aux f = toCNF f

-- On suppose la formule en entrée sous forme CNF
findClauses :: Fml a -> [Fml a]
findClauses (And f g) = findClauses f ++ findClauses g
findClauses f = [f]

{-
fonction toCNF, pour le cas P ou Q
-> P' = toCNF P
-> Q' = toCNF Q
P'' = findClauses P'
Q'' = findClauses Q'
CNF = [Or a b | a <- P'', b <- Q'']

-}

{-
solver :
obtenir liste des clauses,
supposer un litteral vrai ou faux (choix fait si clause unitaire càd une clause avec un unique litteral, ou selon frequence d'occurence),
si le literal
-}
