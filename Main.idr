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
            Nothing => Left $ "Parse error"
      else Left "Empty line to parse."

sampleParser : ParserFunctions [String, Double] ["Name", "Age"]
sampleParser = Just :: parseDouble :: Nil

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

main : IO ()
main =
   do
     y <- run foo
     case y of
          Left str => putStrLn $ "Error: " ++ str
          Right str => putStrLn $ show str
   where
     foo : Eff (Either String (List String)) [FILE_IO(), STDIO]
     foo = do
       x <- readLines "iris.csv"
       pure x -- ?hole2

-- vim: expandtab
