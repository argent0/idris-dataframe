module Main

import Effects
import Effect.File
import Effect.StdIO

import Data.Vect
import Data.String

import DataFrame
import RList

data ParserFunctions : Vect k Type -> Vect k String -> Type where
  Nil : ParserFunctions [] []
  (::) : {n : String} -> (String -> Maybe t) -> ParserFunctions ts ns -> ParserFunctions (t :: ts) (n :: ns)

readLines : String -> Eff (Either String (List String)) [FILE_IO ()]
readLines path = do
      openResult <- open path Read
      case openResult of
         True =>
            (fileLines >>= \fileLinesResult => close >>= \_ => (pure $ Right fileLinesResult))
         False => pure $ Left "Could not open file!"
   where
   fileLines : Eff (List String) [FILE_IO (OpenFile Read)]
   fileLines =
         if !(eof)
            then (pure [])
            else do
              pure (!(readLine) :: !(fileLines))


delimSpan : Char -> String -> (String, String)
delimSpan delim str = 
   let
      (headField, tailFields) = span (/=delim) str
   in (headField, strTail tailFields)

parseDelimRow : {ns : Vect n String} ->
                {ts : Vect n Type} ->
                Char ->
                ParserFunctions ts ns -> String ->
                Either String (RList ts ns)
parseDelimRow _ Nil _ = Right $ Nil
parseDelimRow delim (parsed :: pfs) str =
   if length str > 0
      then
         let
            (field, tailFields) = delimSpan delim str
         in case parsed field of
            Just result =>
               (parseDelimRow delim pfs tailFields) >>= \parsedRest =>
               pure (result :: parsedRest)
            Nothing => Left $ "Parse error at: " ++ str
      else Left "Empty line to parse."


parsedDelimRows : {ns : Vect n String} ->
                  {ts : Vect n Type} ->
                  Char ->
                  ParserFunctions ts ns ->
                  Vect k String ->
                  Either String (DataFrame k ts ns)
parsedDelimRows _ _ Nil = Right empty
parsedDelimRows delim parsers (s :: ss) = do
   parsedRow <- parseDelimRow delim parsers s
   parsedRestRows <- parsedDelimRows delim parsers ss
   pure $ rcons parsedRow parsedRestRows

parseCsv : {ns : Vect n String} ->
           {ts : Vect n Type} ->
           ParserFunctions ts ns ->
           Vect k String ->
           Either String (DataFrame k ts ns)

parseCsv = parsedDelimRows ','

sampleParser : ParserFunctions [String, Double] ["Name", "Age"]
sampleParser = Just :: parseDouble :: Nil

irisParser : ParserFunctions [Double, Double, Double, Double] ["Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"]
irisParser = parseDouble :: parseDouble :: parseDouble :: parseDouble :: Nil

vect : List a -> (k ** Vect k a)
vect Nil = (Z ** [])
vect (x :: xs) =
   let
      (k ** rest) = vect xs
   in (S k ** x :: rest)

main : IO ()
main =
   do 
      runResult <- run fileLinesResult
      case runResult of
         Left str => putStrLn $ "Error: " ++ str
         Right str =>
            let
               (_ ** vstr) = vect (filter ((>0) . length) str)
            in case parseCsv irisParser vstr of
               Left errStr => putStrLn errStr
               Right df => putStrLn $ show df
   where
     fileLinesResult : Eff (Either String (List String)) [FILE_IO(), STDIO]
     fileLinesResult = do
       fileLines <- readLines "iris.headless.csv"
       pure fileLines -- ?hole2

-- vim: expandtab
