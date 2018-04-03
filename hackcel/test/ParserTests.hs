module ParserTests where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Hackcel.Core
import Data.Hackcel.Wrapper.Parser
import Data.Hackcel.Wrapper.Numbers
import Data.Hackcel.Wrapper.NumberTable

doesParse :: String -> Bool
doesParse = snd . parseExpression

shouldParse :: String -> TestTree
shouldParse str = testProperty ("Should parse: " ++ str) $ doesParse str

shouldNotParse :: String -> TestTree
shouldNotParse str = testProperty ("Should not parse: " ++ str) $ not $ doesParse str

valid :: TestTree
valid = testGroup "Parse valid expressions" $ fmap shouldParse
  [ "1"
  , "1.1"
  , "A1"
  , "A10"
  , "ABC1"
  , "1 + 1"
  , " 1 "
  , "A1 * A2 * A3 * A4"
  , "(((1)))"
  , "(A1 + 1) * 3 + 1"
  ]

invalid :: TestTree
invalid = testGroup "Parse with syntax errors" $ fmap shouldNotParse
  [ "1..0"
  , "."
  , "A 1"
  , "1 * * 2"
  , "(1 + 2"
  , "1 A"
  ]

parserTests :: TestTree
parserTests = testGroup "Parser" [valid, invalid]
