module NumberListTests where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Hackcel.Core
import Data.Hackcel.Wrapper.DSL
import Data.Hackcel.Wrapper.NumberList
import Data.Hackcel.Wrapper.Numbers

import Data.Either
import Data.Map.Strict

numberListProperties = testGroup "Number List Properties" [
        singleIntValueProperty,
        singleDoubleValueProperty,
        plusIntPropery,
        minusIntPropery,
        timesIntPropery,
        divideIntNormalPropery,
        divideIntErrorPropery
    ]

fromRight (Right x) = x
fromRight (Left _)  = error "Right expected, got left"

fromLeft (Left x) = x
fromLeft (Right _)  = error "Left expected, got right"

-- Creates a spreadsheet for numbers
createSpreadSheet :: [(Field, Expression Field Value NumberError)] -> HackcelState Field Value NumberError
createSpreadSheet exprs = createHackcel (Spreadsheet $ fromList (exprs)) numberHandler

-- Test whether a value returns the correct response
singleValueProperty :: (Eq a) => (Value -> a) -> (a -> Expression Field Value NumberError) -> a -> Int -> Bool
singleValueProperty unF f x y = (unF valueF) == x
                            where
                                valueF :: Value
                                valueF = (fromRight $ fst result)
                                result :: (Either NumberError Value, HackcelState Field Value NumberError)
                                result = runField (field y) spreadSheet
                                spreadSheet :: HackcelState Field Value NumberError
                                spreadSheet = createSpreadSheet [f x @@ field y]

-- Test the insertion of an Int
singleIntValueProperty = testProperty "Single int insertion and retrieval" $ singleValueProperty fromValueInt valueInt

-- Test the insertion of a Double
singleDoubleValueProperty = testProperty "Single double insertion and retrieval" $ singleValueProperty fromValueDouble valueDouble

-- Verifies that an operation gives the correct result
operationProperty :: (Eq a) => (a -> a -> a) -> String -> (Value -> a) -> (a -> Expression Field Value NumberError) -> a -> a -> Int -> Bool
operationProperty operation opName unF f x y z = (unF valueF) == operation x y
                            where
                                valueF :: Value
                                valueF = (fromRight $ fst result)
                                result :: (Either NumberError Value, HackcelState Field Value NumberError)
                                result = runField (field z) spreadSheet
                                spreadSheet :: HackcelState Field Value NumberError
                                spreadSheet = createSpreadSheet [op opName [PExpr $ f x, PExpr $ f y] @@ field z]

-- Tests the different operators
plusIntPropery = testProperty "Plus Int" (operationProperty (+) "plus" fromValueInt valueInt)
minusIntPropery = testProperty "Minus Int" (operationProperty (-) "minus" fromValueInt valueInt)
timesIntPropery = testProperty "Times Int" (operationProperty (*) "times" fromValueInt valueInt)
divideIntNormalPropery = testProperty "Divide Int by /= 0" ((\x y -> y /= 0 ==> operationProperty div "divide" fromValueInt valueInt x y))

divideIntErrorPropery = testProperty "Divide Int by 0" ((\x y -> errorProperty "divide" (DivideByZeroError "div 0") valueInt x 0 y))

-- | Test whether an error is returned by a property
errorProperty :: (Eq a) => String -> NumberError -> (a -> Expression Field Value NumberError) -> a -> a -> Int -> Bool
errorProperty opName errExp f x y z = errGiv == errExp
                            where
                                errGiv :: NumberError
                                errGiv = (fromLeft $ fst result)
                                result :: (Either NumberError Value, HackcelState Field Value NumberError)
                                result = runField (field z) spreadSheet
                                spreadSheet :: HackcelState Field Value NumberError
                                spreadSheet = createSpreadSheet [op opName [PExpr $ f x, PExpr $ f y] @@ field z]
