{-# LANGUAGE OverloadedStrings #-}

module Parskip where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text

foo :: Parser (String, Int)
foo = do
  _ <- "foo"
  skipMany1 space
  n <- many1 letter
  skipMany1 space
  i <- decimal
  return (n, i)

data Endpoint = Shunt | Hostname Text deriving Show
data Predicate = Predicate {predName:: Text, predArgs :: [Text]} deriving Show
data Filter = Filter {filterName :: Text, filterArgs :: [Text]} deriving Show
data Route = Route {routeId :: Text, predicates :: [Predicate], filters :: [Filter], endpoint :: Endpoint } deriving Show

argsParser :: Parser [Text]
argsParser = do
  skipWhile (/= ')')
  return []

predicateParser :: Parser Predicate
predicateParser = do
  predicateName <- takeWhile1 (inClass "a-zA-Z0-9")
  args <- "(" *> argsParser <* ")"
  return $ Predicate predicateName args

filterParser :: Parser Filter
filterParser = do
  predicateName <- takeWhile1 (inClass "a-zA-Z0-9")
  args <- "(" *> argsParser <* ")"
  return $ Filter predicateName args

endpointParser :: Parser Endpoint
endpointParser = do
  skipSpace *> "->" *> skipSpace
  ("<shunt>" >> return Shunt) <|> do
    endpoint <- "\"" *> takeTill (== '"') <* "\""
    return $ Hostname endpoint

eskip :: Parser [Route]
eskip = do
  routeId <- takeWhile1 (inClass "a-zA-Z0-9")
  skipSpace *> ":" *> skipSpace
  predicates <- predicateParser `sepBy` (skipSpace *> "&&" *> skipSpace)
  skipSpace *> "->" *> skipSpace
  filters <- filterParser `sepBy` (skipSpace *> "->" *> skipSpace)
  endpoint <- endpointParser
  skipSpace *> ";"
  return [Route routeId predicates filters endpoint]

main :: IO ()
main = do
  parseTest eskip "foo: bla(1, \"2\", /3/) && blub() -> filter(1, \"2\", /3/) -> <shunt>;"
  parseTest eskip "foo: bla(1, \"2\", /3/) && blub() -> filter(1, \"2\", /3/) -> \"http://bla/blub\";"
