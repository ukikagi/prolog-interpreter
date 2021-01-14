module Main (main) where

import Parser (int)
import Test.HUnit.Base (Test (TestCase, TestList), assertFailure, (@?=))
import Test.HUnit.Text (runTestTTAndExit)
import Text.Parsec (parse)

testParseIntSuccess :: Test
testParseIntSuccess = TestCase $ parse int "" "12345" @?= Right 12345

testParseIntFail :: Test
testParseIntFail = TestCase $
  case parse int "" "abcde" of
    Left _ -> return ()
    Right _ -> assertFailure "The parse must fail for non-numeric input."

tests :: Test
tests = TestList [testParseIntSuccess, testParseIntFail]

main :: IO ()
main = runTestTTAndExit tests
