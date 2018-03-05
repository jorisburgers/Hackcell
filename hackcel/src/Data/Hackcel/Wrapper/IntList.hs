{-# Language TypeSynonymInstances #-}
{-# Language MultiParamTypeClasses #-}

module Data.Hackcel.Wrapper.IntList where

import Data.Hackcel.Core
import Data.Hackcel.Wrapper.Numbers

import Data.Map.Strict hiding (foldl, map, foldr)

data Field = FieldInt Int
            deriving (Eq, Ord)

instance Show Field where
    show (FieldInt n) = show n



type Expression' = Expression Field Value NumberError

instance HackcelError NumberError Field where
    errorUnknownField field = UnknownFieldError $ "Unknown field error at index " ++ show field
    errorRecursion fields   = RecursionError $ "Circular referencing via " ++ concatMap show fields  
    errorExpectedValueGotRange = ErrorUnexpectedValue
    errorExpectedRangeGotValue = ErrorUnexpectedRange


--handler "sum" [ExprField (FieldInt p1), ExprField (FieldInt p2)] = do
    --let res = map (eval.ExprField) [p1..p2]
    --return $ ValInt $ dif


convertInt :: [Int] -> Map Field (Expression Field Value NumberError, Maybe (FieldResult Field Value NumberError))
convertInt xs = snd $ foldl (\(x,s) y-> (x+1, insert (FieldInt x) (ExprLit (ValInt y), Nothing) s)) (0, empty) xs

convertDouble xs = snd $ foldl (\(x,s) y-> (x+1, insert (FieldInt x) (ExprLit (ValDouble (fromInteger  . toInteger $ y)), Nothing) s)) (0, empty) xs

values n = addExp (n+1) $ convertDouble [0..n]

addExp n s = insert (FieldInt n) (ExprApp "plus" [PExpr $ ExprField (FieldInt 1), PExpr $ ExprField (FieldInt 2)], Nothing) $
             insert (FieldInt $ n+1) (ExprApp "divide" [PExpr $ ExprField (FieldInt 3), PExpr $ ExprField (FieldInt 1)], Nothing) s 


-- fst $ runEval (getValue (FieldInt 12)) (evalState $ FieldInt 0)

hState = HackcelState {
    fields = values 10,
    app = numberHandler
}

evalState field = EvalState{
    esHackcelState = hState
  , esField = field
  , esStack = []
}

prettyprint :: EvalState Field Value NumberError -> String
prettyprint evalS = foldr printField "" $ toAscList allFields
  where
    hackelstate = esHackcelState evalS
    allFields = fields hackelstate

    printField (k, a) s = show k ++ ": " ++ printValue a ++ "\n" ++ s
    printValue (expr, Nothing)  = "=" ++ show expr ++ ": --"
    printValue (expr, Just val) = "=" ++ show expr ++ ": " ++ case fieldValue val of
      Left e -> show e
      Right v -> show v


prettyprinter :: EvalState Field Value NumberError -> IO ()
prettyprinter evalS = do  let res = prettyprint evalS
                          putStrLn res
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
