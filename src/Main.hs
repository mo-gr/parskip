{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text
import Data.Monoid
import Data.Scientific (Scientific, FPFormat(Fixed), isInteger, formatScientific)

data Argument = Textual Text | Numeric Scientific | RegExpic Text deriving Show
data Endpoint = Shunt | Hostname Text deriving Show
data Predicate = CatchAll | Predicate NamedPredicate deriving Show
data NamedPredicate = NamedPredicate {predName:: Text, predArgs :: [Argument]} deriving Show
data Filter = Filter {filterName :: Text, filterArgs :: [Argument]} deriving Show
data Route = Route {routeId :: Text, predicates :: [Predicate], filters :: [Filter], endpoint :: Endpoint } deriving Show

quoteParser :: Char -> (Text -> Argument) -> Parser Argument
quoteParser c constructor = do
   arg <- char c *> takeTill (== c) <* char c
   return $ constructor (snoc (cons c arg) c)

dqParser :: Parser Argument
dqParser = quoteParser '"' Textual

sqParser :: Parser Argument
sqParser = quoteParser '\'' Textual

btParser :: Parser Argument
btParser = quoteParser '`' Textual

reParser :: Parser Argument
reParser = quoteParser '/' RegExpic

numberParser :: Parser Argument
numberParser = fmap Numeric scientific

argsParser :: Parser [Argument]
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
  prettyPrint (Predicate p) = (predName p) <> "(" <> (intercalate ", " (fmap prettyPrint (predArgs p))) <> ")"
  prettyPrint CatchAll = "*"

instance PrettyPrint Filter where
  prettyPrint f = (filterName f) <> "(" <> (intercalate ", " (fmap prettyPrint (filterArgs f))) <> ")"

instance PrettyPrint Route where
  prettyPrint r = (routeId r) <> ": " <> (intercalate " && " $ prettyPrint <$> predicates r) <> "\n"
                  <> (Data.Text.concat $ flip append "\n" <$> append "  -> " <$> prettyPrint <$> filters r)
                  <> "  -> " <> (prettyPrint (endpoint r)) <> ";\n\n"

instance PrettyPrint Argument where
  prettyPrint (RegExpic a) = a
  prettyPrint (Textual a) = a
  prettyPrint (Numeric n) =
    if isInteger n then
      pack (formatScientific Fixed (Just 0) n)
    else
      pack (formatScientific Fixed Nothing n)
