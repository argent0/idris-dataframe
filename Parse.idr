module Parse

import RList
import DataFrame

import Effects
import Effect.File
import Effect.StdIO

import Data.Vect

%access public export
%default total

data ParserFunctions : Vect k Type -> Vect k String -> Type where
  Nil : ParserFunctions [] []
  (::) : {n : String} -> (String -> Maybe t) -> ParserFunctions ts ns -> ParserFunctions (t :: ts) (n :: ns)

partial -- Because the file could be infinite
readLines : String -> Eff (Either String (List String)) [FILE_IO ()]
readLines path = do
      openResult <- open path Read
      case openResult of
         True =>
            (fileLines >>= \fileLinesResult => close >>= \_ => (pure $ Right fileLinesResult))
         False => pure $ Left "Could not open file!"
   where
   partial
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
loadCsv : {ts : Vect n Type} ->
          {ns : Vect n String} ->
          String ->
          (nrows : Nat) ->
          ParserFunctions ts ns ->
          IO (Provider (DataFrame nrows ts ns))

loadCsv filepath nrows parsers =
   do 
      runResult <- run fileLinesResult
      case runResult of
         Left str => pure $ Error str
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
     fileLinesResult : Eff (Either String (List String)) [FILE_IO(), STDIO]
     fileLinesResult = do
       fileLines <- readLines filepath
       pure fileLines
