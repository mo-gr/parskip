{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Attoparsec.Text (parseOnly)
import           Parskip              as E
import           Test.Hspec           (describe, hspec, it, shouldBe)


main :: IO ()
main = hspec $ do
  describe "Parskip parsing" $
    it "should parse a simple eskip expression" $
      parseOnly E.eskipRoute "t: * -> <shunt>;" `shouldBe`
        Right (E.Route "t" [E.CatchAll] [] E.Shunt)
  describe "Parskip printing" $
    it "should pretty print a simple eskip route" $
      E.prettyPrint "" (E.Route "t" [E.CatchAll] [] E.Shunt) `shouldBe`
        "t: *  -> <shunt>;\n"
