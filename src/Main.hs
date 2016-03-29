{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (concat)
import Control.Applicative ((<|>), many)
import Data.Text (Text, pack, unpack, concat, intercalate, append, cons, snoc)
import Data.Monoid ((<>))
import qualified Data.Attoparsec.Text as P

data Endpoint = Shunt | Hostname Text deriving Show
data Predicate = CatchAll | Predicate NamedPredicate deriving Show
data NamedPredicate = NamedPredicate {predName:: Text, predArgs :: [Text]} deriving Show
data Filter = Filter {filterName :: Text, filterArgs :: [Text]} deriving Show
data Route = Route {routeId :: Text, predicates :: [Predicate], filters :: [Filter], endpoint :: Endpoint } deriving Show

quoteParser :: Char -> P.Parser Text
quoteParser c = do
   arg <- P.char c *> P.takeTill (== c) <* P.char c
   return $ snoc (cons c arg) c

dqParser :: P.Parser Text
dqParser = quoteParser '"'

sqParser :: P.Parser Text
sqParser = quoteParser '\''

btParser :: P.Parser Text
btParser = quoteParser '`'

reParser :: P.Parser Text
reParser = quoteParser '/'

floatParser :: P.Parser Text
floatParser = do
    beforeDot <- P.decimal :: P.Parser Int
    _ <- P.char '.'
    afterDot <- P.decimal :: P.Parser Int
    return $ pack (show beforeDot) <> "." <> pack (show afterDot)

integerParser :: P.Parser Text
integerParser = do
    int <- P.decimal :: P.Parser Int
    return $ pack (show int)

numberParser :: P.Parser Text
numberParser = floatParser <|> integerParser

argsParser :: P.Parser [Text]
argsParser = (numberParser
              <|> dqParser
              <|> sqParser
              <|> btParser
              <|> reParser
             ) `P.sepBy` (P.skipSpace *> "," *> P.skipSpace)

predicateParser :: P.Parser Predicate
predicateParser = catchAllParser <|> namedPredicateParser

catchAllParser :: P.Parser Predicate
catchAllParser = "*" >> return CatchAll

namedPredicateParser :: P.Parser Predicate
namedPredicateParser = do
  predicateName <- P.takeWhile1 (P.inClass "a-zA-Z0-9")
  args <- "(" *> argsParser <* ")"
  return $ Predicate $ NamedPredicate predicateName args

filterParser :: P.Parser Filter
filterParser = do
  predicateName <- P.takeWhile1 (P.inClass "a-zA-Z0-9")
  args <- "(" *> argsParser <* ")"
  return $ Filter predicateName args

endpointParser :: P.Parser Endpoint
endpointParser = ("<shunt>" >> return Shunt) <|> do
    endpoint' <- "\"" *> P.takeTill (== '"') <* "\""
    return $ Hostname endpoint'

eskipRoute :: P.Parser Route
eskipRoute = do
  routeId' <- P.skipSpace *> P.takeWhile1 (P.inClass "a-zA-Z0-9")
  P.skipSpace *> ":" *> P.skipSpace
  predicates' <- predicateParser `P.sepBy` (P.skipSpace *> "&&" *> P.skipSpace)
  P.skipSpace *> "->" *> P.skipSpace
  filters' <- P.option [] (filterParser `P.sepBy` (P.skipSpace *> "->" *> P.skipSpace) <* P.skipSpace <* "->" <* P.skipSpace)
  endpoint' <- endpointParser
  P.skipSpace *> ";" *> P.skipSpace *> P.skipMany P.endOfLine
  return $ Route routeId' predicates' filters' endpoint'

commentLine :: P.Parser ()
commentLine = do
  P.skipWhile (not . P.isEndOfLine)
  return ()

parseAndFormat :: String -> String
parseAndFormat input = let routesOrError = P.parseOnly (many eskipRoute) (pack input) in
                           either id (unpack . concat . fmap prettyPrint) routesOrError

main :: IO ()
main = interact parseAndFormat

test :: IO ()
test = do
  let pRoute = prettyPrint <$> P.parseOnly eskipRoute "foo: bla(1) && blub() -> foo() -> bar(/reg/) -> <shunt>;"
  either print (putStrLn.unpack) pRoute
  P.parseTest eskipRoute "foo: * -> <shunt>;"
  P.parseTest eskipRoute "foo: bla(1) -> <shunt>;"
  P.parseTest eskipRoute "foo: bla(0.67) -> <shunt>;"
  P.parseTest eskipRoute "foo: bla(`1`) -> <shunt>;"
  P.parseTest eskipRoute "foo: bla(1, \"2\", /3/) && blub() -> filter(1, \"2\", /3/) -> <shunt>;"
  P.parseTest eskipRoute "foo: bla(1, \"2\", /3/) && blub() -> filter(1, \"2\", /3/) -> \"http://bla/blub\";"

class PrettyPrint a where
  prettyPrint :: a -> Text

instance PrettyPrint Endpoint where
  prettyPrint Shunt = "<shunt>"
  prettyPrint (Hostname h) = "\"" <> h <> "\""

instance PrettyPrint Predicate where
  prettyPrint (Predicate p) = predName p <> "(" <> intercalate ", " (predArgs p) <> ")"
  prettyPrint CatchAll = "*"

instance PrettyPrint Filter where
  prettyPrint f = filterName f <> "(" <> intercalate ", " (filterArgs f) <> ")"

instance PrettyPrint Route where
  prettyPrint r = routeId r <> ": " <> intercalate " && " (prettyPrint <$> predicates r) <> "\n"
                  <> concat (flip append "\n" <$> append "  -> " <$> prettyPrint <$> filters r)
                  <> "  -> " <> prettyPrint (endpoint r) <> ";\n\n"
