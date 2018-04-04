{-# Language TypeSynonymInstances #-}
{-# Language MultiParamTypeClasses #-}
module Data.Hackcel.Wrapper.NumberTable where

import Data.Hackcel.Core
import Data.Hackcel.Wrapper.Numbers
import Data.Hackcel.Wrapper.DSL

import Data.Either
import Data.Map.Strict hiding (foldl, map)

import Prelude hiding (LT, GT, EQ)

data Field = FieldInt (Int, Int)
            deriving (Eq, Ord)

type Expression' = Expression Field Value NumberError Fns

field :: (Int, Int) -> Field
field = FieldInt

fieldExpr :: (Int, Int) -> Expression Field Value NumberError Fns
fieldExpr = ExprField . field

fieldParam :: (Int, Int) -> Parameter Field Value NumberError Fns
fieldParam = PExpr . fieldExpr

instance Show Field where
    show (FieldInt n) = show n

instance HackcelError NumberError Field where
    errorUnknownField field = UnknownFieldError $ "Unknown field error at index " ++ show field
    errorRecursion fields = RecursionError $ "Circular referencing via " ++ concatMap show fields
    errorExpectedValueGotRange = ErrorUnexpectedValue
    errorExpectedRangeGotValue = ErrorUnexpectedRange

instance FieldRange Field where
    getRange (FieldInt (x1, y1)) (FieldInt (x2, y2)) = [FieldInt (x, y)  | x <- [(min x1 x2)..(max x1 x2)]
                                                                , y <- [(min y1 y2) .. (max y1 y2)]]

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
                [op Plus [fieldParam (0,2), PExpr $ valueInt 3], valueInt 6, valueDouble 3.5],
                [valueInt 1, valueInt 2, op Sum [PRange (field (0, 0)) (field (2, 1))]],
                [valueInt 2, valueInt 2, op LT [fieldParam (3, 0), (fieldParam (3, 1))]

                ]
            ]

getField f = fst $ runField f (listToSpreadSheet expressions)
