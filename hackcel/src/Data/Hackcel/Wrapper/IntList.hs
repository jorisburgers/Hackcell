{-# Language TypeSynonymInstances #-}
{-# Language MultiParamTypeClasses #-}

module Data.Hackcel.Wrapper.IntList where

import Data.Hackcel.Core

import Data.Map.Strict hiding (foldl, map)

data Field = FieldInt Int
            deriving (Eq, Ord)

instance Show Field where
    show (FieldInt n) = show n

data Value = ValInt Int
            deriving Show

data IntListError   = RecursionError String
                    | UnknownFieldError String
                    | DivideByZeroError String
                    deriving Show


instance HackcelError IntListError Field where
    errorUnknownField field = UnknownFieldError $ "Unknown error at index " ++ show field
    errorRecursion fields   = RecursionError $ "Circular referencing via " ++ concatMap show fields    

type Eval' = Eval Field Value IntListError Value
type Expression' = Expression Field Value IntListError

eval :: Expression' -> Eval'
eval = undefined

opHandler :: (Int -> Int -> Int) -> [Expression'] -> Eval'
opHandler op [ex, ey] = do
                ValInt x <- eval ex
                ValInt y <- eval ey
                return $ ValInt $ op x y

handler :: String -> [Expression'] -> Eval'
handler "plus" xs = opHandler (+) xs
handler "minus" xs = opHandler (-) xs
handler "times" xs = opHandler (*) xs
handler "divide" [ex, ey] = do
    ValInt x <- eval ex
    ValInt y <- eval ey
    if y == 0 then
        return $ ValInt 0 --throwError (DivideByZeroError "Divide by zero")
    else
        return $ ValInt $ div x y

--handler "sum" [ExprField (FieldInt p1), ExprField (FieldInt p2)] = do
    --let res = map (eval.ExprField) [p1..p2]
    --return $ ValInt $ dif


convert :: [Int] -> Map Field (Expression Field Value IntListError, Maybe (FieldResult Field Value IntListError))
convert xs = snd $ foldl (\(x,s) y-> (x+1, insert (FieldInt x) (ExprLit (ValInt y), Nothing) s)) (0, empty) xs

values n = convert [2..n] --addExp (n+1) $ convert [1..n]

addExp n s = insert (FieldInt n) (ExprApp "plus" [ExprField (FieldInt 1), ExprField (FieldInt 2)], Nothing) $
             insert (FieldInt $ n+1) (ExprApp "minus" [ExprField (FieldInt 3), ExprField (FieldInt 4)], Nothing) s 

hState = HackcelState {
    fields = values 10,
    app = handler
}

evalState field = EvalState{
    esHackcelState = hState
  , esField = field
  , esStack = []
}