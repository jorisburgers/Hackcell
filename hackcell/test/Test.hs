module Main where

import Test.Tasty

import NumberListTests
import NumberTableTests
import InvalidateTests
import ParserTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
        properties,
        unitTests
    ]

properties :: TestTree
properties = testGroup "Properties" [
        numberListProperties,
        numberTableProperties
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [invalidateTests, parserTests]
