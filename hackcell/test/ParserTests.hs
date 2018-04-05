module ParserTests where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Hackcell.Core
import Data.Hackcell.Wrapper.Parser
import Data.Hackcell.Wrapper.Numbers
import Data.Hackcell.Wrapper.NumberTable

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
  , "1 == 1 && 2 == 2"
  , "1 == 1 && 2 == 2 || 3 == 4"
  , "!!!!!!!!!2"
  , "1 ? 2 : 3"
  , "1 ? 2 ? 3 : 4 : 5 ? 6 : 7"
  , "Sum(A1;C10)"
  , "Sum(A1;C10) + Sum(X9;Y10)"
  , "True"
  , "True == True && False == False ? True : False"
  , "True != False"
  , "1 + 2 * 3"
  , "1 * 2 + 3"
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
