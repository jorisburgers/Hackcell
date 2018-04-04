{-# Language TypeSynonymInstances #-}
{-# Language MultiParamTypeClasses #-}
module Data.Hackcel.Wrapper.NumberTable where

import Data.Hackcel.Core
import Data.Hackcel.Wrapper.Numbers
import Data.Hackcel.Wrapper.DSL

import Data.Either
import Data.Map.Strict hiding (foldl, map)

data Field = FieldInt (Int, Int)
            deriving (Eq, Ord)

type Expression' = Expression Field Value NumberError Fns

field :: (Int, Int) -> Field
field = FieldInt

fieldExpr :: (Int, Int) -> Expression Field Value NumberError Fns
fieldExpr = ExprField . field

instance Show Field where
    show (FieldInt n) = show n

instance HackcelError NumberError Field where
    errorUnknownField field = UnknownFieldError $ "Unknown field error at index " ++ show field
    errorRecursion fields = RecursionError $ "Circular referencing via " ++ concatMap show fields  
    errorExpectedValueGotRange = ErrorUnexpectedValue
    errorExpectedRangeGotValue = ErrorUnexpectedRange

listToSpreadSheet :: [[Expression Field Value NumberError Fns]] -> HackcelState Field Value NumberError Fns
listToSpreadSheet xss   | not (sameLengths xss) = error "multidimensionale array not all the same size"
                        | otherwise             = createHackcel (Spreadsheet $ fromList $ concat $ fields xss 0 0) 
                        where
                            sameLengths :: [[a]] -> Bool
                            sameLengths []  = True
                            sameLengths xss = and $ map (\xs -> length (head xss) == length xs) xss
                            fieldCel    x c r           = x @@ field (r, c)
                            fieldRow    [] c r          = []
                            fieldRow    (x:xs) c r      = fieldCel x c r : fieldRow xs (c+1) r
                            fields      []  c r         = []
                            fields      (xs:xss) c r    = fieldRow xs 0 r : fields xss 0 (r+1)

                       

expressions = [
                [valueInt 3, valueInt 5, valueInt 7], 
                [op Plus [fieldExpr (0,2), valueInt 3], valueDouble 3.5, valueInt 6]
            ]