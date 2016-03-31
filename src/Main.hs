{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (concat)
import Control.Applicative ((<|>), many)
import Data.Text (Text, pack, unpack, concat, intercalate, append, cons, snoc)
import Data.Monoid ((<>))
import System.Environment (getArgs)
import Data.Scientific (Scientific, FPFormat(Fixed), isInteger, formatScientific)
import Data.Attoparsec.Text (Parser, skipSpace, skipMany, skipWhile, char, takeTill, takeWhile1, scientific, sepBy, inClass, option, endOfLine, isEndOfLine, parseOnly, parseTest)

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
numberParser = Numeric <$> scientific

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
endpointParser = ("<shunt>" >> return Shunt) <|> do
    endpoint' <- "\"" *> takeTill (== '"') <* "\""
    return $ Hostname endpoint'

eskipRoute :: Parser Route
eskipRoute = do
  routeId' <- skipSpace *> takeWhile1 (inClass "a-zA-Z0-9")
  skipSpace *> ":" *> skipSpace
  predicates' <- predicateParser `sepBy` (skipSpace *> "&&" *> skipSpace)
  skipSpace *> "->" *> skipSpace
  filters' <- option [] (filterParser `sepBy` (skipSpace *> "->" *> skipSpace) <* skipSpace <* "->" <* skipSpace)
  endpoint' <- endpointParser
  skipSpace *> ";" *> skipSpace *> skipMany endOfLine
  return $ Route routeId' predicates' filters' endpoint'

commentLine :: Parser ()
commentLine = do
  skipWhile (not . isEndOfLine)
  return ()

transformArg :: [String] -> Text
transformArg ("--compact":_) = ""
transformArg _ = "\n"

parseAndFormat :: Text -> String -> String
parseAndFormat sep input = let routesOrError = parseOnly (many eskipRoute) (pack input) in
                           either id (unpack . concat . fmap (prettyPrint sep)) routesOrError

main :: IO ()
main = do
  args <- getArgs
  interact $ parseAndFormat $ transformArg args

test :: IO ()
test = do
  let pRoute = prettyPrint "" <$> parseOnly eskipRoute "foo: bla(1) && blub(1.0) -> foo(1.1) -> bar(/reg/) -> <shunt>;"
  either print (putStrLn.unpack) pRoute
  parseTest eskipRoute "foo: * -> <shunt>;"
  parseTest eskipRoute "foo: bla(1) -> <shunt>;"
  parseTest eskipRoute "foo: bla(0.67) -> <shunt>;"
  parseTest eskipRoute "foo: bla(`1`) -> <shunt>;"
  parseTest eskipRoute "foo: bla(1, \"2\", /3/) && blub() -> filter(1, \"2\", /3/) -> <shunt>;"
  parseTest eskipRoute "foo: bla(1, \"2\", /3/) && blub() -> filter(1, \"2\", /3/) -> \"http://bla/blub\";"

class PrettyPrint a where
  prettyPrint :: Text -> a -> Text

instance PrettyPrint Endpoint where
  prettyPrint _ Shunt = "<shunt>"
  prettyPrint _ (Hostname h) = "\"" <> h <> "\""

instance PrettyPrint Predicate where
  prettyPrint newLine (Predicate p) = predName p <> "(" <> intercalate ", " (prettyPrint newLine <$> predArgs p) <> ")"
  prettyPrint _ CatchAll = "*"

instance PrettyPrint Filter where
  prettyPrint newLine f = filterName f <> "(" <> intercalate ", " (prettyPrint newLine <$> filterArgs f) <> ")"

instance PrettyPrint Route where
  prettyPrint newLine r =
        routeId r <> ": " <> predicates' <>
        filters' <> "  -> " <>
        endpoint' <>
        end'
    where
      pr :: PrettyPrint a => a -> Text
      pr = prettyPrint newLine
      predicates' = intercalate " && " (pr <$> predicates r) <> newLine
      filters' = concat (flip append newLine <$> append "  -> " <$> pr <$> filters r)
      endpoint' = pr (endpoint r)
      end' = concat [";", "\n", newLine]

instance PrettyPrint Argument where
  prettyPrint _ (RegExpic a) = a
  prettyPrint _ (Textual a)  = a
  prettyPrint _ (Numeric n)  = pack (formatScientific Fixed (t n) n)
    where
      t n
        | isInteger n = Just 0
        | otherwise   = Nothing

