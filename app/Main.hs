module Main where

import qualified Data.List as L
import qualified Data.Algorithm.Sat.Fml.Examples as Ex
import qualified Data.Algorithm.Sat.Fml.Model as Model
import qualified Data.Algorithm.Sat.Utils as Utils
import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Lit as Lit
import qualified Data.Algorithm.Sat.Var as Var
import qualified Data.Algorithm.Sat.Solver as Solver
import qualified Data.Algorithm.Sat.Assignment as Assignment
import qualified Data.Algorithm.Sat.Query as Query
import qualified Data.Algorithm.Sat.Solver.CNFFml as CNFFml


fmlPrinter :: Fml.Fml Char -> String
fmlPrinter a = ""
  ++ "\n  Raw formula : " ++ (Fml.prettyPrinter a)
  {-
  ++ "\n  Reduced : " ++ (Fml.prettyPrinter $ Fml.reduce a)
  ++ "\n  ToCNF : " ++ (Fml.prettyPrinter $ Fml.toCNF a)
  ++ "\n  Lits : " ++ (show . Utils.uniques . CNFFml.litList $ preProcessed)
  ++ "\n  All lits : " ++ (show . CNFFml.litList $ preProcessed)
  ++ "\n  Clauses : " ++ (show . CNFFml.getClauses $ preProcessed)
  ++ "\n  Lit to process : " ++ (show . CNFFml.findLitToProcess $ preProcessed)
  ++ "\n  Simplified : " ++ (show . CNFFml.getClauses $ CNFFml.simplify preProcessed (lit $ CNFFml.findLitToProcess preProcessed))
  -}
  ++ "\n"
  ++ "\n  Found " ++ (show . length $ Fml.uniqueVars a) ++ " different vars ..."
  ++ "\n  Found " ++ (show . length $ Fml.vars a) ++ " total vars ..."
  ++ "\n  Found " ++ (show . length . Utils.uniques . CNFFml.litList $ preProcessed) ++ " different lits ..."
  ++ "\n  Found " ++ (show . length . CNFFml.litList $ preProcessed) ++ " total lits ..."
  ++ "\n  Found " ++ (show . length . CNFFml.getClauses $ preProcessed) ++ " clauses ..."
  ++ "\n"
  ++ "\n  Satisfiable ? " ++ (show $ Query.satisfiable a)
  ++ "\n  Solution : " ++ (showSolution $ Solver.solve a)
  ++ "\n  Tautology ? " ++ (show $ Query.tautology a)
  ++ "\n"
  ++ "\n  Constrained vars : " ++ (show $ Query.constrainedVars (solution $ Solver.solve a))
  ++ "\n  Not constrained vars : " ++ (show $ Query.notConstrainedVars a (solution $ Solver.solve a))
  ++ "\n  Possible assignments : " ++ (show . length $ Query.satisfyingAssignments a)
  ++ "\n  -> " ++ (show $ Query.satisfyingAssignments a)

  ++ "\n\n"
  where
    preProcessed = Solver.preProcess a
    solution (Just a) = a
    solution (Nothing) = Assignment.mkEmpty
    showSolution (Just a) = show a
    showSolution (Nothing) = "No solution found"
    lit (Just b) = b
    lit Nothing = error("@#&% !!")

main :: IO ()
main = putStrLn "Sat solver test :\n\n"
  >> putStrLn ("fml1") >> putStrLn (fmlPrinter Ex.fml1)
  >> putStrLn ("fml2") >> putStrLn (fmlPrinter Ex.fml2)
  >> putStrLn ("fml3") >> putStrLn (fmlPrinter Ex.fml3)
  >> putStrLn ("fml4") >> putStrLn (fmlPrinter Ex.fml4)
  >> putStrLn ("fml5") >> putStrLn (fmlPrinter Ex.fml5)
  >> putStrLn ("fml6") >> putStrLn (fmlPrinter Ex.fml6)
  >> putStrLn ("fml7") >> putStrLn (fmlPrinter Ex.fml7)
  >> putStrLn ("fml8") >> putStrLn (fmlPrinter Ex.fml8)
  >> putStrLn ("fml9") >> putStrLn (fmlPrinter Ex.fml9)
  {-
  >> putStrLn ("fml10") >> putStrLn (fmlPrinter Ex.fml10)
  >> putStrLn ("fml11") >> putStrLn (fmlPrinter Ex.fml11)
  >> putStrLn ("fml12") >> putStrLn (fmlPrinter Ex.fml12)
  >> putStrLn ("fml13") >> putStrLn (fmlPrinter Ex.fml13)
  -}
