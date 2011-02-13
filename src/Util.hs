module Util where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token

separateBy :: Char -> String -> Maybe [String]
separateBy sep input = 
   case parse parseCommas "(unknown)" input of
      Left _ -> Nothing
      Right x -> Just x
   where
      parseCommas :: Parser [String]
      parseCommas = sepBy (many1 (noneOf [sep])) (char sep)

separateCommas :: String -> Maybe [String]
separateCommas = separateBy ','

-- Use this when you error out and you knew it was possible that the error would
-- occur. Use it to let the user know that this is expected behaviour considering what the program
-- could find.
gracefulExit :: IO ()
gracefulExit = putStrLn "hTodo shutdown gracefully."
