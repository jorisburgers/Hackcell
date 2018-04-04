module Main where

import Test.Tasty

import NumberListTests
import NumberTableTests
import ParserTests

main = defaultMain tests

tests = testGroup "Tests" [
        properties, 
        unitTests
    ]

properties = testGroup "Properties" [
        numberListProperties,
        numberTableProperties
    ]

unitTests = testGroup "Unit tests" [parserTests]
