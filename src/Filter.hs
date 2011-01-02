module Filter (
   FilterType(..),
   Filter(..),
   Range(..),
   getFilters
   ) where

import Text.ParserCombinators.Parsec

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

data Range a
   = SingletonRange a
   | SpanRange a a
   deriving(Eq, Show)

getFilters :: String -> Either ParseError [Filter]
getFilters = parse parseFilters "(filter)"

parseFilters :: CharParser st [Filter]
parseFilters = sepBy1 parseFilter (char ',')

parseFilter :: CharParser st Filter
parseFilter 
   = try (templateFilter "all" FilterAll)
   <|> try (templateFilter "children" FilterChildren)
   <|> try (templateFilter "done" FilterDone)
   <|> try indexRanges
   where
      templateFilter :: String -> (FilterType -> Filter) -> CharParser st Filter
      templateFilter name constructor = do
         ft <- parseFilterType
         string name
         return $ constructor ft

      indexRanges :: CharParser st Filter
      indexRanges = do
         ft <- parseFilterType
         ranges <- sepBy1 indexRange (char '.')
         return FilterIndex { filterType = ft, filterRange = ranges }
         where
            indexRange :: CharParser st (Range Integer)
            indexRange = do
               first <- many1 digit
               decide first

            decide :: String -> CharParser st (Range Integer)
            decide first = 
               (do
                  char '-'
                  second <- many1 digit 
                  return (SpanRange (read first) (read second))
               )
               <|> return (SingletonRange (read first))

      parseFilterType :: CharParser st FilterType
      parseFilterType 
         = (char '+' >> return FilterTypeAdd)
         <|> (char '-' >> return FilterTypeRemove)
         <|> (char '=' >> return FilterTypeEquals)
         <|> return FilterTypeDefault
