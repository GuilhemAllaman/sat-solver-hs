module Main where

import qualified Data.Algorithm.Sat.Fml.Examples as Ex
import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Solver.CNFFml as CNFFml

fmlPrinter :: Show a => Fml.Fml a -> String
fmlPrinter a = "Raw :\n"
  ++ (Fml.prettyPrinter a)
  ++ "\nToCNF :\n"
  ++ (Fml.prettyPrinter . Fml.toCNF $ a)
  ++ "\nClauses :\n"
  ++ (show . CNFFml.getClauses . CNFFml.fmlToCNFFml . Fml.toCNF $ a)
  ++ "\n"

main :: IO ()
main = putStrLn ("fml1") >> putStrLn (fmlPrinter Ex.fml1)
  >> putStrLn ("fml2") >> putStrLn (fmlPrinter Ex.fml2)
  >> putStrLn ("fml3") >> putStrLn (fmlPrinter Ex.fml3)
  >> putStrLn ("fml4") >> putStrLn (fmlPrinter Ex.fml4)
  >> putStrLn ("fml5") >> putStrLn (fmlPrinter Ex.fml5)
