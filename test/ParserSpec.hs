module ParserSpec (spec) where

import Data.Either (isLeft)
import Parser (int, term)
import Syntax (Term (TComp, TVar, Wild), tAtom)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Parsec (parse)

spec :: Spec
spec = do
  describe "Parser.int" $ do
    it "parses numerics" $ do
      parse int "" "12345" `shouldBe` Right (12345 :: Int)

    it "fails for non-numerics" $ do
      parse int "" "aaa" `shouldSatisfy` isLeft

  describe "Parser.term" $ do
    it "parses a compound term" $ do
      parse term "" "rel(ika, tako)" `shouldBe` Right (TComp "rel" [tAtom "ika", tAtom "tako"])

    it "parses a variable" $ do
      parse term "" "Xvariable" `shouldBe` Right (TVar "Xvariable")

    it "parses a wildcard" $ do
      parse term "" "_" `shouldBe` Right Wild