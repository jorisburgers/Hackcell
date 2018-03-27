{-# Language TypeSynonymInstances #-}
{-# Language MultiParamTypeClasses #-}

module Data.Hackcel.Wrapper.NumberList where



import Data.Hackcel.Core
import Data.Hackcel.Wrapper.Numbers
import Data.Hackcel.Wrapper.DSL

import Data.Either
import Data.Map.Strict hiding (foldl, map, foldr)
import Text.Read

data Field = FieldInt Int
            deriving (Eq, Ord)

field :: Int -> Field
field = FieldInt

fieldExpr :: Int -> Expression Field Value NumberError
fieldExpr = ExprField . field

instance Show Field where
    show (FieldInt n) = show n

instance HackcelError NumberError Field where
    errorUnknownField field = UnknownFieldError $ "Unknown field error at index " ++ show field
    errorRecursion fields   = RecursionError $ "Circular referencing via " ++ concatMap show fields
    errorExpectedValueGotRange = ErrorUnexpectedValue
    errorExpectedRangeGotValue = ErrorUnexpectedRange

listToSpreadSheet :: [Expression Field Value NumberError] -> HackcelState Field Value NumberError
listToSpreadSheet xs = createHackcel (Spreadsheet $ fromList fields) numberHandler
                    where
                        fields :: [(Field, Expression Field Value NumberError)]
                        fields = map (\(x, i) -> x @@ field i) $
                                    fst $ foldl (\x y -> (fst x ++ [(y, snd x)], snd x + 1)) ([], 0) xs

{-
    example call:
    fst $ runEval (dependencies (FieldInt 12)) $ snd $ runEval (getValue (FieldInt 12)) (evalState $ FieldInt 0)
    returns the array of 3, 4, the values that 12 depends on
    fst $ runEval (dependencies (FieldInt 11)) $ snd $ runEval (getValue (FieldInt 11)) (evalState $ FieldInt 0)
    returns the array of 1, 2
    fst $ runEval (dependencies (FieldInt 9)) $ snd $ runEval (getValue (FieldInt 9)) (evalState $ FieldInt 0)
    returns [], as it has no dependencies
    fst $ runEval (dependencies (FieldInt 12)) $ snd $ runEval (getValue (FieldInt 9)) (evalState $ FieldInt 0)
    returns [], as 12 is not yet calculated and has therefore no dependencies.
-}

values = map (\x -> valueInt x @@ field x) [0..10]

computations =
    [
        op "plus" [fieldExpr 3, fieldExpr 5] @@ field 11,
        op "plus" [fieldExpr 11, fieldExpr 8] @@ field 12,
        op "divide" [fieldExpr 3, valueInt 0] @@ field 13,
        op "minus" [valueInt 3, valueInt 5] @@ field 14
    ]

spreadSheet = createHackcel (Spreadsheet $ fromList (values ++ computations)) numberHandler

expressions = [valueInt 3, valueInt 5, valueInt 7, op "plus" [fieldExpr 2, fieldExpr 0], valueDouble 3.5]

foundValues :: [Either NumberError Value]
foundValues = fst $ getValues (map field [0..14]) spreadSheet

intParser :: String -> Maybe Field
intParser s = FieldInt <$> readMaybe s

efParser :: String -> Maybe (Field, Expression Field Value NumberError)
efParser s = Just $ (op "plus" [fieldExpr 3, fieldExpr 5] @@ field 16)
