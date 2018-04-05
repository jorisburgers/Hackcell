{-# Language TypeSynonymInstances #-}
{-# Language MultiParamTypeClasses #-}
-- | Gives a wrapper implementation for a single dimensional spreadsheet for `Numbers`
module Data.Hackcel.Wrapper.NumberList(
        Field(..)
    ,   field
    ,   fieldExpr
    ,   fieldParam
    ,   listToSpreadSheet
) where



import Data.Hackcel.Core
import Data.Hackcel.Wrapper.Numbers
import Data.Hackcel.Wrapper.DSL

import Data.Either
import Data.Map.Strict hiding (foldl, map, foldr)
import Text.Read

-- | Describes a field in a single dimension
data Field = Field Int -- ^ Constructs a single field
            deriving (Eq, Ord)

-- | Creates a field based on the given Int
field :: Int -> Field
field = Field

-- | Given an Int, creates an expression of that field
fieldExpr :: Int -> Expression Field Value NumberError Fns
fieldExpr = ExprField . field

-- | Given an Int, creates a Parameter of that field
fieldParam :: Int -> Parameter Field Value NumberError Fns
fieldParam = PExpr . fieldExpr

instance Show Field where
    show (Field n) = show n

instance FieldRange Field where
    getRange (Field x) (Field y) = map Field [x..y]

instance HackcelError NumberError Field where
    errorUnknownField field = UnknownFieldError $ "Unknown field error at index " ++ show field
    errorRecursion fields   = RecursionError $ "Circular referencing via " ++ concatMap show fields
    errorExpectedValueGotRange = ErrorUnexpectedValue
    errorExpectedRangeGotValue = ErrorUnexpectedRange

-- | Creates a one dimensional spreadsheet from a list of expressions
listToSpreadSheet :: [Expression Field Value NumberError Fns] -> HackcelState Field Value NumberError Fns
listToSpreadSheet xs = createHackcel (Spreadsheet $ fromList fields)
                    where
                        fields :: [(Field, Expression Field Value NumberError Fns)]
                        fields = map (\(x, i) -> x @@ field i) $
                                    fst $ foldl (\x y -> (fst x ++ [(y, snd x)], snd x + 1)) ([], 0) xs
