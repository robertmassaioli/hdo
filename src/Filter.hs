module Filter 
   ( FilterType(..)
   , Filter(..)
   , Range(..)
   , getFilters
   , parseRanges
   ) where

import Text.Parsec
import Text.Parsec.String
import Range

data FilterType 
   = FilterTypeAdd 
   | FilterTypeEquals 
   | FilterTypeRemove
   | FilterTypeDefault
   deriving(Eq, Show)

data Filter 
   = FilterAll 
      { filterType :: FilterType 
      }
   | FilterChildren
      { filterType :: FilterType 
      }
   | FilterDone
      { filterType :: FilterType 
      }
   | FilterPriority
      { filterType :: FilterType 
      , filterPriority :: Integer
      }
   | FilterIndex
      { filterType :: FilterType 
      , filterRange :: [Range Integer]
      }
   | FilterRegex
      { regexString :: String
      }
   deriving(Eq, Show)

getFilters :: String -> Either ParseError [Filter]
getFilters = parse parseFilters "(filter)"

parseFilters :: Parser [Filter]
parseFilters = sepBy1 parseFilter (char ',')

parseFilter :: Parser Filter
parseFilter 
   = try (templateFilter "all" FilterAll)
   <|> try (templateFilter "children" FilterChildren)
   <|> try (templateFilter "done" FilterDone)
   <|> try indexRanges
   where
      templateFilter :: String -> (FilterType -> Filter) -> Parser Filter
      templateFilter name constructor = do
         ft <- parseFilterType
         string name
         return $ constructor ft

      indexRanges :: Parser Filter
      indexRanges = do
         ft <- parseFilterType
         ranges <- parseRanges '.'
         return FilterIndex { filterType = ft, filterRange = ranges }

      parseFilterType :: Parser FilterType
      parseFilterType 
         = (char '+' >> return FilterTypeAdd)
         <|> (char '-' >> return FilterTypeRemove)
         <|> (char '=' >> return FilterTypeEquals)
         <|> return FilterTypeDefault

parseRanges :: (Ord a, Read a) => Char -> Parser [Range a]
parseRanges c = sepBy1 parseRange (char c)
   where 
      parseRange :: (Ord a, Read a) => Parser (Range a)
      parseRange = do
         first <- many1 digit
         decide first

      decide :: (Ord a, Read a) => String -> Parser (Range a)
      decide first = 
         (do
            char '-'
            second <- many1 digit 
            return (SpanRange (read first) (read second))
         )
         <|> return (SingletonRange (read first))
