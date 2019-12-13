module Main where

import qualified Data.Algorithm.Sat.Fml.Examples as Ex
import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Lit as Lit
import qualified Data.Algorithm.Sat.Var as Var
import qualified Data.Algorithm.Sat.Solver.CNFFml as CNFFml
import qualified Data.Algorithm.Sat.Solver as Solver

fmlPrinter :: Fml.Fml Char -> String
fmlPrinter a = "\nRaw :\n"
  ++ (Fml.prettyPrinter a)
  ++ "\nReduced :\n"
  ++ (Fml.prettyPrinter $ Fml.reduce a)
  ++ "\nToCNF :\n"
  ++ (Fml.prettyPrinter $ Fml.toCNF a)
  ++ "\nLits :\n"
  ++ (show . CNFFml.litList $ propered)
  ++ "\nLit to process :\n"
  ++ (show . CNFFml.findLitToProcess $ propered)
  ++ "\nClauses :\n"
  ++ (show . CNFFml.getClauses $ propered)
  ++ "\n"
  ++ "simplify \n"
  ++ (show . CNFFml.getClauses $ CNFFml.simplified propered )
  ++ "\n\n"
  where propered = Solver.preProcess a

main :: IO ()
main = putStrLn ("fml1") >> putStrLn (fmlPrinter Ex.fml1)
  >> putStrLn ("fml2") >> putStrLn (fmlPrinter Ex.fml2)
  >> putStrLn ("fml3") >> putStrLn (fmlPrinter Ex.fml3)
  >> putStrLn ("fml4") >> putStrLn (fmlPrinter Ex.fml4)
  >> putStrLn ("fml5") >> putStrLn (fmlPrinter Ex.fml5)
  >> putStrLn ("fml6") >> putStrLn (fmlPrinter Ex.fml6)
  >> putStrLn ("fml7") >> putStrLn (fmlPrinter Ex.fml7)
  >> putStrLn ("fml8") >> putStrLn (fmlPrinter Ex.fml8)
  >> putStrLn ("fml9") >> putStrLn (fmlPrinter Ex.fml9)
