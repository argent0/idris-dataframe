module Parse

import DataFrame.RList
import DataFrame.DataFrame

import Control.ST.File
import Control.ST

import Data.Vect

%access public export
%default total

data ParserFunctions : Vect k Type -> Vect k String -> Type where
  Nil : ParserFunctions [] []
  (::) : {n : String} -> (String -> Maybe t) -> ParserFunctions ts ns -> ParserFunctions (t :: ts) (n :: ns)


partial
getLines : File m => 
         (fh : Var) -> 
         Either FileError (List String) -> 
         ST m (Either FileError (List String)) [fh ::: FileHandleI {m = m} Read]
getLines file strings = do
  case strings of
    Left err => pure (Left err)
    Right accum => do
      if !(eof file) then
        pure (Right accum)
      else do
        line <- readLine file
        case line of 
          Left err => pure (Left err)
          Right str => getLines file (Right (str :: accum))
      
                                                                                                   
partial -- Because the file could be infinite
readLines : File m => String -> ST m (Either FileError (List String)) []
readLines path = do
  openResult <- open path Read
  case openResult of
    Left x => pure (Left x)
    Right fh => do
     lines <- getLines fh (Right [])
     close fh
     pure lines

delimSpan : Char -> String -> (String, String)
delimSpan delim str = 
   let
      (headField, tailFields) = span (/=delim) str
   in case strM tailFields of
           StrNil => (headField, "")
           StrCons _ rest => (headField, rest)

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

private
vect : List a -> (k ** Vect k a)
vect Nil = (Z ** [])
vect (x :: xs) =
   let
      (k ** rest) = vect xs
   in (S k ** x :: rest)

partial
loadCsv : (File m) => 
          {ts : Vect n Type} ->
          {ns : Vect n String} ->
          String ->
          (nrows : Nat) ->
          ParserFunctions ts ns ->
          ST m (Provider (DataFrame nrows ts ns)) []

loadCsv filepath nrows parsers =
   do 
      runResult <- fileLinesResult
      case runResult of
         Left err => pure $ Error (show err)
         Right str =>
            let
               (n ** vstr) = vect (filter ((>0) . length) str)
            in case parseCsv parsers vstr of
               Left errStr => pure $ Error errStr
               Right df => case exactRowNumber nrows df of
                                Just rightDf => pure $ Provide rightDf
                                Nothing => pure $ Error "Missing rows!"
   where
     partial
     fileLinesResult : (File m) => ST m (Either FileError (List String)) []
     fileLinesResult = do
       fileLines <- readLines filepath
       pure fileLines
