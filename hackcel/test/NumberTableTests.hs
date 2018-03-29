module NumberTableTests where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Hackcel.Core
import Data.Hackcel.Wrapper.DSL
import Data.Hackcel.Wrapper.NumberTable
import Data.Hackcel.Wrapper.Numbers

import Data.Map.Strict hiding (foldl, map)

numberTableProperties = testGroup "Number Table Properties" [
        singleIntValueProperty,
        singleDoubleValueProperty
    ]

fromRight (Right x) = x
fromRight (Left _)  = error "Right expected, got left"

fromLeft (Left x) = x
fromLeft (Right _)  = error "Left expected, got right"

-- Creates a spreadsheet for numbers
createSpreadSheet :: [[(Field, Expression Field Value NumberError)]] -> HackcelState Field Value NumberError
createSpreadSheet exprs = createHackcel (Spreadsheet $ fromList $ concat exprs) numberHandler

-- Test whether a value returns the correct response
singleValueProperty :: (Eq a) => (Value -> a) -> (a -> Expression Field Value NumberError) -> a -> Int -> Int -> Bool
singleValueProperty unF f x y z = (unF valueF) == x
                            where
                                valueF :: Value
                                valueF = (fromRight $ fst result)
                                result :: (Either NumberError Value, HackcelState Field Value NumberError)
                                result = runField (field (y, z)) spreadSheet
                                spreadSheet :: HackcelState Field Value NumberError
                                spreadSheet = createSpreadSheet [[f x @@ field (y, z)]]

-- Test the insertion of an Int
singleIntValueProperty = testProperty "Single int insertion and retrieval" $ singleValueProperty fromValueInt valueInt

-- Test the insertion of a Double
singleDoubleValueProperty = testProperty "Single int insertion and retrieval" $ singleValueProperty fromValueDouble valueDouble
