{-# LANGUAGE OverloadedStrings #-}

module Parskip (eskipRoute, prettyPrint, Route(..), Filter(..), NamedPredicate(..), Predicate(..), Endpoint(..)) where

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text (Parser, char, endOfLine, inClass,
                                       isEndOfLine, option, scientific, sepBy,
                                       skipMany, skipSpace, skipWhile, takeTill,
                                       takeWhile1)
import           Data.Monoid          ((<>))
import           Data.Scientific      (FPFormat (Fixed), Scientific,
                                       formatScientific, isInteger)
import           Data.Text            (Text, append, concat, cons, intercalate,
                                       pack, snoc)
import           Prelude              hiding (concat)

data Argument = Textual Text | Numeric Scientific | RegExpic Text deriving (Show, Eq)
data Endpoint = Shunt | Hostname Text deriving (Show, Eq)
data Predicate = CatchAll | Predicate NamedPredicate deriving (Show, Eq)
data NamedPredicate = NamedPredicate {predName:: Text, predArgs :: [Argument]} deriving (Show, Eq)
data Filter = Filter {filterName :: Text, filterArgs :: [Argument]} deriving (Show, Eq)
data Route = Route {routeId :: Text, predicates :: [Predicate], filters :: [Filter], endpoint :: Endpoint } deriving (Show, Eq)

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
      t n'
        | isInteger n' = Just 0
        | otherwise    = Nothing

