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
multOr (x:xs) = Or x (multOr xs)


reduce :: Fml a -> Fml a
reduce (Imply a b) = Or (Not $ reduce a) $ reduce b
reduce (Equiv a b) = And (Or (Not $ reduce a) $ reduce b) $ Or (Not $ reduce b) $ reduce a
reduce (XOr a b) = Or (And (reduce a) $ Not (reduce b)) (And (reduce b) $ Not (reduce a))
reduce (Or a b) = Or (reduce a)  (reduce b)
reduce (And a b) = And (reduce a) ( reduce b)
reduce (Not a) = Not $ reduce a
reduce (Final a) = Final a


toCNF :: Fml a -> Fml a
toCNF = aux . reduce
  where
    aux (Final a) = Final a
    aux (And a b) = And (aux a) (aux b)
    aux (Imply a b) = aux $ Or (Not a) b
    aux (Equiv a b) = aux $ Or (And a b) (And (Not a) (Not b))
    aux (XOr a b) = aux $ Or (And a (Not b)) (And (Not a) b)
    aux (Or a b) = multAnd [Or p q | p <- auxOr a, q <- auxOr b]
      where
        auxOr = findClauses . aux
    aux (Not a) = auxNot a
      where
        auxNot (Final a) = Not (Final a)
        auxNot (Not a) = aux a
        auxNot (And a b) = aux $ Or (Not a) (Not b)
        auxNot (Or a b) = aux $ And (Not a) (Not b)
        auxNot f = Not $ aux f

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
