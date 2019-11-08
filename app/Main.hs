module Main where

import qualified Data.Algorithm.Sat.Fml.Examples as Ex
import qualified Data.Algorithm.Sat.Fml as Fml

main :: IO ()
main = putStrLn (Fml.prettyPrinter Ex.fml1)
  >> putStrLn (show (Fml.vars Ex.fml1))
  >> putStrLn (Fml.prettyPrinter Ex.fml2)
  >> putStrLn (show (Fml.vars Ex.fml2))
  >> putStrLn (Fml.prettyPrinter Ex.fml3)
  >> putStrLn (show (Fml.vars Ex.fml3))
