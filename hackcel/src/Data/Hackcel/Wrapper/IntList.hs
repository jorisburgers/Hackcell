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
        return $ ValInt 0 --throwError (DivideByZeroError "Divide by zero")
    else
        return $ ValInt $ div x y

--handler "sum" [ExprField (FieldInt p1), ExprField (FieldInt p2)] = do
    --let res = map (eval.ExprField) [p1..p2]
    --return $ ValInt $ dif


convert :: [Int] -> Map Field (Expression Field Value IntListError, Maybe (FieldResult Field Value IntListError))
convert xs = snd $ foldl (\(x,s) y-> (x+1, insert (FieldInt x) (ExprLit (ValInt y), Nothing) s)) (0, empty) xs

values n = addExp (n+1) $ convert [1..n]

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