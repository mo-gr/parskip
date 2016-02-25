{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text

data Endpoint = Shunt | Hostname Text deriving Show
data Predicate = Predicate {predName:: Text, predArgs :: [Text]} deriving Show
data Filter = Filter {filterName :: Text, filterArgs :: [Text]} deriving Show
data Route = Route {routeId :: Text, predicates :: [Predicate], filters :: [Filter], endpoint :: Endpoint } deriving Show

quoteParser :: Char -> Parser Text
quoteParser c = do
   arg <- char c *> takeTill (== c) <* char c
   return $ snoc (cons c arg) c

dqParser :: Parser Text
dqParser = quoteParser '"'

sqParser :: Parser Text
sqParser = quoteParser '\''

btParser :: Parser Text
btParser = quoteParser '`'

reParser :: Parser Text
reParser = quoteParser '/'

numberParser :: Parser Text
numberParser = do
  num <- scientific
  return $ pack (show num)

argsParser :: Parser [Text]
argsParser = (numberParser
              <|> dqParser
              <|> sqParser
              <|> btParser
              <|> reParser
             ) `sepBy` (skipSpace *> "," *> skipSpace)

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
  ("<shunt>" >> return Shunt) <|> do
    endpoint <- "\"" *> takeTill (== '"') <* "\""
    return $ Hostname endpoint

eskipRoute :: Parser Route
eskipRoute = do
  routeId <- skipSpace *> takeWhile1 (inClass "a-zA-Z0-9")
  skipSpace *> ":" *> skipSpace
  predicates <- predicateParser `sepBy` (skipSpace *> "&&" *> skipSpace)
  skipSpace *> "->" *> skipSpace
  filters <- option [] (filterParser `sepBy` (skipSpace *> "->" *> skipSpace) <* skipSpace <* "->" <* skipSpace)
  endpoint <- endpointParser
  skipSpace *> ";" *> skipSpace *> skipMany endOfLine
  return $ Route routeId predicates filters endpoint

parseAndFormat :: String -> String
parseAndFormat input = let routesOrError = parseOnly (many eskipRoute) (pack input) in
                           either id (unpack . Data.Text.concat . (fmap prettyPrint)) routesOrError

main :: IO ()
main = interact parseAndFormat

test :: IO ()
test = do
  let pRoute = prettyPrint <$> parseOnly eskipRoute "foo: bla(1) && blub() -> foo() -> bar(/reg/) -> <shunt>;"
  either print (putStrLn.unpack) pRoute
  parseTest eskipRoute "foo: bla(1) -> <shunt>;"
  parseTest eskipRoute "foo: bla(`1`) -> <shunt>;"
  parseTest eskipRoute "foo: bla(1, \"2\", /3/) && blub() -> filter(1, \"2\", /3/) -> <shunt>;"
  parseTest eskipRoute "foo: bla(1, \"2\", /3/) && blub() -> filter(1, \"2\", /3/) -> \"http://bla/blub\";"

class PrettyPrint a where
  prettyPrint :: a -> Text

instance PrettyPrint Endpoint where
  prettyPrint Shunt = "<shunt>"
  prettyPrint (Hostname h) = "\"" `append` h `append` "\""

instance PrettyPrint Predicate where
  prettyPrint p = (predName p) `append` "(" `append` (intercalate ", " (predArgs p)) `append` ")"

instance PrettyPrint Filter where
  prettyPrint f = (filterName f) `append` "(" `append` (intercalate ", " (filterArgs f)) `append` ")"

instance PrettyPrint Route where
  prettyPrint r = (routeId r) `append` ": " `append` (intercalate " && " $ prettyPrint <$> predicates r) `append` "\n"
                  `append` (Data.Text.concat $ flip append "\n" <$> append "  -> " <$> prettyPrint <$> filters r)
                  `append` "  -> " `append` (prettyPrint (endpoint r)) `append` ";\n\n"
