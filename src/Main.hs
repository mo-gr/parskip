{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Applicative  (many)
import           Data.Attoparsec.Text (parseOnly)
import           Data.Text            (Text, concat, pack, unpack)
import           Parskip              (eskipRoute, prettyPrint)
import           Prelude              hiding (concat)
import           System.Environment   (getArgs)

transformArg :: [String] -> Text
transformArg ("--compact":_) = ""
transformArg ("-c":_) = ""
transformArg _ = "\n"

parseAndFormat :: Text -> String -> String
parseAndFormat sep input = let routesOrError = parseOnly (many eskipRoute) (pack input) in
                           either id (unpack . concat . fmap (prettyPrint sep)) routesOrError

main :: IO ()
main = do
  args <- getArgs
  interact $ parseAndFormat $ transformArg args
