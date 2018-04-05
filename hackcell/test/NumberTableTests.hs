module NumberTableTests where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Hackcell.Core
import Data.Hackcell.Wrapper.DSL
import Data.Hackcell.Wrapper.NumberTable
import Data.Hackcell.Wrapper.Numbers

import Data.Map.Strict hiding (foldl, map)

numberTableProperties :: TestTree
numberTableProperties = testGroup "Number Table Properties" [
        singleIntValueProperty,
        singleDoubleValueProperty
    ]

fromRight' :: Either a b -> b
fromRight' (Right x) = x
fromRight' (Left _)  = error "Right expected, got left"

-- Creates a spreadsheet for numbers
createSpreadSheet :: [[(Field, Expression Field Value NumberError Fns)]] -> HackcellState Field Value NumberError Fns
createSpreadSheet exprs = createHackcell (Spreadsheet $ fromList $ concat exprs)

-- Test whether a value returns the correct response
singleValueProperty :: (Eq a) => (Value -> a) -> (a -> Expression Field Value NumberError Fns) -> a -> Int -> Int -> Bool
singleValueProperty unF f x y z = (unF valueF) == x
                            where
                                valueF :: Value
                                valueF = (fromRight' $ fst result)
                                result :: (Either NumberError Value, HackcellState Field Value NumberError Fns)
                                result = runField (field (y, z)) spreadSheet
                                spreadSheet :: HackcellState Field Value NumberError Fns
                                spreadSheet = createSpreadSheet [[f x @@ field (y, z)]]

-- Test the insertion of an Int
singleIntValueProperty :: TestTree
singleIntValueProperty = testProperty "Single int insertion and retrieval" $ singleValueProperty fromValueInt valueInt

-- Test the insertion of a Double
singleDoubleValueProperty :: TestTree
singleDoubleValueProperty = testProperty "Single int insertion and retrieval" $ singleValueProperty fromValueDouble valueDouble
