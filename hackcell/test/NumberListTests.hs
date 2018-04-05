module NumberListTests where

import Prelude hiding (EQ, LT, GT)

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Hackcell.Core
import Data.Hackcell.Wrapper.DSL
import Data.Hackcell.Wrapper.NumberList
import Data.Hackcell.Wrapper.Numbers hiding (fromValueInt, fromValueBool, fromValueDouble)

import Data.Map.Strict

numberListProperties :: TestTree
numberListProperties = testGroup "Number List Properties" [
        singleIntValueProperty,
        singleDoubleValueProperty,
        plusIntProperty,
        minusIntProperty,
        timesIntProperty,
        divideIntNormalProperty,
        divideIntErrorProperty,
        andBoolProperty,
        orBoolProperty,
        ltIntProperty,
        leIntProperty,
        gtIntProperty,
        geIntProperty
    ]

fromValueInt :: Value -> Int
fromValueInt (ValInt x) = x
fromValueInt _          = error "Value is not an Int"

fromValueDouble :: Value -> Double
fromValueDouble (ValDouble x)   = x
fromValueDouble _               = error "Value is not a Double"

fromValueBool :: Value -> Bool
fromValueBool (ValBool x) = x
fromValueBool _          = error "Value is not a Boolean"

fromRight' :: Either a b -> b
fromRight' (Right x) = x
fromRight' (Left _)  = error "Right expected, got left"

fromLeft' :: Either a b -> a
fromLeft' (Left x) = x
fromLeft' (Right _)  = error "Left expected, got right"

-- Creates a spreadsheet for numbers
createSpreadSheet :: [(Field, Expression Field Value NumberError Fns)] -> HackcellState Field Value NumberError Fns
createSpreadSheet exprs = createHackcell $ Spreadsheet $ fromList exprs

-- Test whether a value returns the correct response
singleValueProperty :: (Eq a) => (Value -> a) -> (a -> Expression Field Value NumberError Fns) -> a -> Int -> Bool
singleValueProperty unF f x y = (unF valueF) == x
                            where
                                valueF :: Value
                                valueF = (fromRight' $ fst result)
                                result :: (Either NumberError Value, HackcellState Field Value NumberError Fns)
                                result = runField (field y) spreadSheet
                                spreadSheet :: HackcellState Field Value NumberError Fns
                                spreadSheet = createSpreadSheet [f x @@ field y]

-- Test the insertion of an Int
singleIntValueProperty :: TestTree
singleIntValueProperty = testProperty "Single int insertion and retrieval" $ singleValueProperty fromValueInt valueInt

-- Test the insertion of a Double
singleDoubleValueProperty :: TestTree
singleDoubleValueProperty = testProperty "Single double insertion and retrieval" $ singleValueProperty fromValueDouble valueDouble

-- Verifies that an operation gives the correct result
operationProperty :: (Eq b) => (a -> a -> b) -> Fns -> (Value -> b) -> (a -> Expression Field Value NumberError Fns) -> a -> a -> Int -> Bool
operationProperty operation opName unF f x y z = (unF valueF) == operation x y
                            where
                                valueF :: Value
                                valueF = (fromRight' $ fst result)
                                result :: (Either NumberError Value, HackcellState Field Value NumberError Fns)
                                result = runField (field z) spreadSheet
                                spreadSheet :: HackcellState Field Value NumberError Fns
                                spreadSheet = createSpreadSheet [op opName [PExpr $ f x, PExpr $ f y] @@ field z]

-- Tests the different operators
plusIntProperty :: TestTree
plusIntProperty = testProperty "Plus Int" (operationProperty (+) Plus fromValueInt valueInt)
minusIntProperty :: TestTree
minusIntProperty = testProperty "Minus Int" (operationProperty (-) Minus fromValueInt valueInt)
timesIntProperty :: TestTree
timesIntProperty = testProperty "Times Int" (operationProperty (*) Times fromValueInt valueInt)
andBoolProperty :: TestTree
andBoolProperty = testProperty "And Bool" (operationProperty (&&) And fromValueBool valueBool)
orBoolProperty :: TestTree
orBoolProperty = testProperty "Or Bool" (operationProperty (||) Or fromValueBool valueBool)
ltIntProperty :: TestTree
ltIntProperty = testProperty "LT Int" (operationProperty (<) LT fromValueBool valueInt)
leIntProperty :: TestTree
leIntProperty = testProperty "LE Int" (operationProperty (<=) LE fromValueBool valueInt)
gtIntProperty :: TestTree
gtIntProperty = testProperty "GT Int" (operationProperty (>) GT fromValueBool valueInt)
geIntProperty :: TestTree
geIntProperty = testProperty "GE Int" (operationProperty (>=) GE fromValueBool valueInt)
divideIntNormalProperty :: TestTree
divideIntNormalProperty = testProperty "Divide Int by /= 0" ((\x y -> y /= 0 ==> operationProperty div Divide fromValueInt valueInt x y))
divideIntErrorProperty :: TestTree
divideIntErrorProperty = testProperty "Divide Int by 0" ((\x y -> errorProperty Divide (DivideByZeroError "div 0") valueInt x 0 y))

-- | Test whether an error is returned by a property
errorProperty :: (Eq a) => Fns -> NumberError -> (a -> Expression Field Value NumberError Fns) -> a -> a -> Int -> Bool
errorProperty opName errExp f x y z = errGiv == errExp
                            where
                                errGiv :: NumberError
                                errGiv = (fromLeft' $ fst result)
                                result :: (Either NumberError Value, HackcellState Field Value NumberError Fns)
                                result = runField (field z) spreadSheet
                                spreadSheet :: HackcellState Field Value NumberError Fns
                                spreadSheet = createSpreadSheet [op opName [PExpr $ f x, PExpr $ f y] @@ field z]
