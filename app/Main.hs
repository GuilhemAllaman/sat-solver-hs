module Main where

import qualified Data.Algorithm.Sat.Fml.Examples as Ex
import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Lit as Lit
import qualified Data.Algorithm.Sat.Var as Var
import qualified Data.Algorithm.Sat.Solver as Solver
import qualified Data.Algorithm.Sat.Query as Query
import qualified Data.Algorithm.Sat.Solver.CNFFml as CNFFml


fmlPrinter :: Fml.Fml Char -> String
fmlPrinter a = "\nRaw :\n"
  ++ (Fml.prettyPrinter a)
  ++ "\n  Reduced :\n"
  ++ (Fml.prettyPrinter $ Fml.reduce a)
  ++ "\n  ToCNF :\n"
  ++ (Fml.prettyPrinter $ Fml.toCNF a)
  ++ "\n  All lits :\n"
  ++ (show . CNFFml.litList $ preProcessed)
  ++ "\n  Clauses :\n"
  ++ (show . CNFFml.getClauses $ preProcessed)
  ++ "\n  Lit to process :\n"
  ++ (show . CNFFml.findLitToProcess $ preProcessed)
  ++ "\n  Simplified :\n"
  ++ (show . CNFFml.getClauses $ CNFFml.simplified preProcessed)
  ++ "\n  Satisfiable ?\n"
  ++ (show $ Query.satisfiable a)
  ++ "\n  Solved :\n"
  ++ (solution $ Solver.solve a)
  ++ "\n\n"
  where
    preProcessed = Solver.preProcess a
    solution (Just a) = show a
    solution (Nothing) = "No solution found"

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
