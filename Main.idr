module Main

import Effects
import Effect.File
import Effect.StdIO

readLines : String -> Eff (Either String (List String)) [FILE_IO (), STDIO]
--readLines : String -> Eff (Either String (List String)) [FILE_IO ()]
readLines path = do
      openResult <- open path Read
      case openResult of
         True =>
            (fileLines [] >>= \fileLinesResult => close >>= \_ => (pure $ Right fileLinesResult))
         False => pure $ Left "Could not open file!"
   where
   fileLines : List String -> Eff (List String) [FILE_IO (OpenFile Read)]
   fileLines acc =
         if !(eof)
            then (pure acc)
            else do
              thisLine <- readLine
              fileLines (thisLine :: acc)


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
       x <- readLines "DataFrame.idr"
       pure x -- ?hole2
