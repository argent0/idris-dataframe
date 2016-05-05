module Main

import DataFrame
import Parse

import Effects
import Effect.File
import Effect.StdIO

import Data.Vect
import Data.String

%language TypeProviders

irisParser : ParserFunctions [Double, Double, Double, Double] ["Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"]
irisParser = parseDouble :: parseDouble :: parseDouble :: parseDouble :: Nil

%provide (irisDF : (DataFrame 150 [Double, Double, Double, Double] ["Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"])) with
   loadCsv "iris.headless.csv" 150 irisParser

-- %provide (irisDF : (DataFrame 10 [Double, Double, Double, Double] ["Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"])) with
--    loadCsv "iris.head.csv" 10 irisParser

main : IO ()
main = putStrLn $ show irisDF

-- vim: expandtab
