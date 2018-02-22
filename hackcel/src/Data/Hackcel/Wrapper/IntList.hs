{-# Language TypeSynonymInstances #-}
{-# Language MultiParamTypeClasses #-}

module Data.Hackcel.Wrapper.IntList where

import Data.Hackcel.Core

import Data.Map.Strict hiding (foldl)

data Field = FieldInt Int
            deriving (Eq, Ord)

instance Show Field where
    show (FieldInt n) = show n

data Value = ValInt Int

data IntListError   = RecursionError String
                    | UnknownFieldError String
                    | DivideByZeroError String


instance HackcelError IntListError Field where
    errorUnknownField field = UnknownFieldError $ "Unknown error at index " ++ show field
    errorRecursion fields   = RecursionError $ "Circular referencing via " ++ concatMap show fields    

type Eval' = Eval Field Value IntListError Value

opHandler :: (Int -> Int -> Int) -> [Eval'] -> Eval'
opHandler op [ex, ey] = do
                ValInt x <- ex
                ValInt y <- ey
                return $ ValInt $ op x y

handler :: String -> [Eval'] -> Eval'
handler "plus" xs = opHandler (+) xs
handler "minus" xs = opHandler (-) xs
handler "times" xs = opHandler (*) xs
handler "divide" [ex, ey] = do
    ValInt x <- ex
    ValInt y <- ey
    if y == 0 then
        evalError (DivideByZeroError "Divide by zero")
    else
        return $ ValInt $ div x y

convert :: [Int] -> Map Field (Expression Field Value IntListError, Maybe (FieldResult Field Value IntListError))
convert xs = snd $ foldl (\(x,s) y-> (x+1, insert (FieldInt x) (ExprLit (ValInt y), Nothing) s)) (0, empty) xs

values n = addExp (n+1) $ convert [1..n]

addExp n s = insert (FieldInt n) (ExprApp "plus" [ExprField (FieldInt 1), ExprField (FieldInt 2)], Nothing) $
             insert (FieldInt $ n+1) (ExprApp "minus" [ExprField (FieldInt 3), ExprField (FieldInt 4)], Nothing) s 

hState = HackcelState {
    fields = values 10,
    app = handler
}