module Main

import DataFrame
import Data.Vect
import Data.String
import Control.ST.File
import Control.ST

%language TypeProviders

irisParser : ParserFunctions [Double, Double, Double, Double] ["Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"]
irisParser = parseDouble :: parseDouble :: parseDouble :: parseDouble :: Nil

%provide (irisDF : (DataFrame 150 [Double, Double, Double, Double] ["Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"])) with
   run $ loadCsv {m = IO} "iris.headless.csv" 150 irisParser

main : IO ()
main = putStrLn $ show irisDF

-- vim: expandtab
