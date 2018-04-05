{-# Language TypeSynonymInstances #-}
{-# Language MultiParamTypeClasses #-}
module Data.Hackcel.Wrapper.NumberTable where

import Data.Hackcel.Core
import Data.Hackcel.Wrapper.DSL
import Data.Hackcel.Wrapper.Numbers

import Data.Either
import qualified Data.Map.Strict as M
import Data.Char


import Prelude hiding (LT, GT, EQ)

data Field = FieldInt (Int, Int)
            deriving (Eq, Ord)

type Expression' = Expression Field Value NumberError Fns

field :: (Int, Int) -> Field
field = FieldInt

fieldExpr :: (Int, Int) -> Expression'
fieldExpr = ExprField . field

fieldParam :: (Int, Int) -> Parameter Field Value NumberError Fns
fieldParam = PExpr . fieldExpr

instance Show Field where
    show (FieldInt (a,b)) = columntoNumber a ++ show b

columntoNumber :: Int -> String
columntoNumber x | x < 0 = '-' : helper (-x)
                 | x == 0 = "o"
                 | otherwise = helper x
  where
    helper :: Int -> String
    helper 0 = ""
    helper x = let (d, m) = divMod (x - 1) 26 in
               helper d ++ [chr (ord 'A' + m)]



instance HackcelError NumberError Field where
    errorUnknownField field = UnknownFieldError $ "Unknown field error at index " ++ show field
    errorRecursion fields   = RecursionError $ "Circular referencing via " ++ concatMap ((++" -> ") . show) fields ++ " ..."
    errorExpectedValueGotRange = ErrorUnexpectedValue
    errorExpectedRangeGotValue = ErrorUnexpectedRange

instance FieldRange Field where
    getRange (FieldInt (x1, y1)) (FieldInt (x2, y2)) = [FieldInt (x, y)  | x <- [(min x1 x2)..(max x1 x2)]
                                                                , y <- [(min y1 y2) .. (max y1 y2)]]

listToSpreadSheet :: [[Expression']] -> HackcelState Field Value NumberError Fns
listToSpreadSheet xss   | not (sameLengths xss) = error "multidimensionale array not all the same size"
                        | otherwise             = createHackcel (Spreadsheet $ M.fromList $ concat $ fields xss 1 1)
                        where
                            sameLengths :: [[a]] -> Bool
                            sameLengths []  = True
                            sameLengths xss = all (\xs -> length (head xss) == length xs) xss
                            fieldCel    x c r           = x @@ field (r, c)
                            fieldRow    [] c r          = []
                            fieldRow    (x:xs) c r      = fieldCel x c r : fieldRow xs (c+1) r
                            fields      []  c r         = []
                            fields      (xs:xss) c r    = fieldRow xs c r : fields xss c (r+1)

expressions = [
                [valueInt 3, valueInt 5, valueInt 7],
                [op Plus [fieldParam (0,2), PExpr $ valueInt 3], valueInt 6, valueDouble 3.5],
                [valueInt 1, valueInt 2, op Sum [PRange (field (0, 0)) (field (2, 1))]],
                [valueInt 2, valueInt 2, op LT [fieldParam (3, 0), (fieldParam (3, 1))]

                ]
            ]

getField f = fst $ runField f (listToSpreadSheet expressions)
