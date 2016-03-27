{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text
import Data.Monoid

data Endpoint = Shunt | Hostname Text deriving Show
data Predicate = CatchAll | Predicate NamedPredicate deriving Show
data NamedPredicate = NamedPredicate {predName:: Text, predArgs :: [Text]} deriving Show
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

floatParser :: Parser Text
floatParser = do
    beforeDot <- decimal
    char '.'
    afterDot <- decimal
    return $ (pack (show beforeDot)) <> "." <> (pack (show afterDot))

integerParser :: Parser Text
integerParser = do
    int <- decimal
    return $ pack (show int)

numberParser :: Parser Text
numberParser = do
  num <- floatParser <|> integerParser
  return $ num

argsParser :: Parser [Text]
argsParser = (numberParser
              <|> dqParser
              <|> sqParser
              <|> btParser
              <|> reParser
             ) `sepBy` (skipSpace *> "," *> skipSpace)

predicateParser :: Parser Predicate
predicateParser = catchAllParser <|> namedPredicateParser

catchAllParser :: Parser Predicate
catchAllParser = "*" >> return CatchAll

namedPredicateParser :: Parser Predicate
namedPredicateParser = do
  predicateName <- takeWhile1 (inClass "a-zA-Z0-9")
  args <- "(" *> argsParser <* ")"
  return $ Predicate $ NamedPredicate predicateName args

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

commentLine :: Parser ()
commentLine = do
  skipWhile (\c -> not $ isEndOfLine c)
  return ()

parseAndFormat :: String -> String
parseAndFormat input = let routesOrError = parseOnly (many eskipRoute) (pack input) in
                           either id (unpack . Data.Text.concat . (fmap prettyPrint)) routesOrError

main :: IO ()
main = interact parseAndFormat

test :: IO ()
test = do
  let pRoute = prettyPrint <$> parseOnly eskipRoute "foo: bla(1) && blub() -> foo() -> bar(/reg/) -> <shunt>;"
  either print (putStrLn.unpack) pRoute
  parseTest eskipRoute "foo: * -> <shunt>;"
  parseTest eskipRoute "foo: bla(1) -> <shunt>;"
  parseTest eskipRoute "foo: bla(0.67) -> <shunt>;"
  parseTest eskipRoute "foo: bla(`1`) -> <shunt>;"
  parseTest eskipRoute "foo: bla(1, \"2\", /3/) && blub() -> filter(1, \"2\", /3/) -> <shunt>;"
  parseTest eskipRoute "foo: bla(1, \"2\", /3/) && blub() -> filter(1, \"2\", /3/) -> \"http://bla/blub\";"

class PrettyPrint a where
  prettyPrint :: a -> Text

instance PrettyPrint Endpoint where
  prettyPrint Shunt = "<shunt>"
  prettyPrint (Hostname h) = "\"" <> h <> "\""

instance PrettyPrint Predicate where
  prettyPrint (Predicate p) = (predName p) <> "(" <> (intercalate ", " (predArgs p)) <> ")"
  prettyPrint CatchAll = "*"

instance PrettyPrint Filter where
  prettyPrint f = (filterName f) <> "(" <> (intercalate ", " (filterArgs f)) <> ")"

instance PrettyPrint Route where
  prettyPrint r = (routeId r) <> ": " <> (intercalate " && " $ prettyPrint <$> predicates r) <> "\n"
                  <> (Data.Text.concat $ flip append "\n" <$> append "  -> " <$> prettyPrint <$> filters r)
                  <> "  -> " <> (prettyPrint (endpoint r)) <> ";\n\n"
