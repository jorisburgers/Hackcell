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

createSpreadSheet :: [(Field, Expression Field Value NumberError)] -> HackcelState Field Value NumberError
createSpreadSheet exprs = createHackcel (Spreadsheet $ fromList (exprs)) numberHandler

singleValueProperty :: (Eq a) => (Value -> a) -> (a -> Expression Field Value NumberError) -> a -> Int -> Bool
singleValueProperty unF f x y = (unF valueF) == x
                            where
                                valueF :: Value
                                valueF = (fromRight (error "No value was returned") $ fst result)
                                result :: (Either NumberError Value, HackcelState Field Value NumberError)
                                result = runField (field y) spreadSheet
                                spreadSheet :: HackcelState Field Value NumberError
                                spreadSheet = createSpreadSheet [f x @@ field y]

singleIntValueProperty = testProperty "Single int insertion and retrieval" $ singleValueProperty fromValueInt valueInt

singleDoubleValueProperty = testProperty "Single int insertion and retrieval" $ singleValueProperty fromValueDouble valueDouble

operationProperty :: (Eq a) => (a -> a -> a) -> String -> (Value -> a) -> (a -> Expression Field Value NumberError) -> a -> a -> Int -> Bool
operationProperty operation opName unF f x y z = (unF valueF) == operation x y
                            where
                                valueF :: Value
                                valueF = (fromRight (error "No value was returned") $ fst result)
                                result :: (Either NumberError Value, HackcelState Field Value NumberError)
                                result = runField (field z) spreadSheet
                                spreadSheet :: HackcelState Field Value NumberError
                                spreadSheet = createSpreadSheet [op opName [f x, f y] @@ field z]

plusIntPropery = testProperty "Plus Int" (operationProperty (+) "plus" fromValueInt valueInt)
minusIntPropery = testProperty "Minus Int" (operationProperty (-) "minus" fromValueInt valueInt)
timesIntPropery = testProperty "Times Int" (operationProperty (*) "times" fromValueInt valueInt)
divideIntNormalPropery = testProperty "Divide Int by /= 0" ((\x y -> y /= 0 ==> operationProperty div "divide" fromValueInt valueInt x y))

divideIntErrorPropery = testProperty "Divide Int by 0" ((\x y -> errorProperty "divide" (DivideByZeroError "div 0") valueInt x 0 y))

errorProperty :: (Eq a) => String -> NumberError -> (a -> Expression Field Value NumberError) -> a -> a -> Int -> Bool
errorProperty opName errExp f x y z = errGiv == errExp
                            where
                                errGiv :: NumberError
                                errGiv = (fromLeft (error "No error was returned but expected")) $ fst result
                                result :: (Either NumberError Value, HackcelState Field Value NumberError)
                                result = runField (field z) spreadSheet
                                spreadSheet :: HackcelState Field Value NumberError
                                spreadSheet = createSpreadSheet [op opName [f x, f y] @@ field z]

