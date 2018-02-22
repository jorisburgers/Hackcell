{-# language MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Hackcel.Core where

import Control.Monad
import Data.Map.Strict as M
import Control.Monad.Except as E
import Control.Monad.State.Lazy as S

newtype Spreadsheet field value error = Spreadsheet { unSpreadsheet :: Map field (Expression field value error) }

data HackcelState field value error = HackcelState
  { fields :: Map field (Expression field value error, Maybe (FieldResult field value error))
  , app :: String -> [Expression field value error] -> Eval field value error value
  }

data FieldResult field value error = FieldResult
  { fieldValue :: Either error value
  , fieldDependants :: [field]
  }

class HackcelError t field where
  errorUnknownField :: field -> t
  errorRecursion :: [field] -> t

data Expression field value error
  = ExprField field
  | ExprLit value
  | ExprLetIn String (Expression field value error) (Expression field value error)
  | ExprApp String [Expression field value error]

data EvalState field value error = EvalState
  { esHackcelState :: HackcelState field value error
  , esField :: field
  , esStack :: [field]
  }

newtype Eval field value error a = Eval {
  runEval :: E.ExceptT error (S.State (EvalState field value error)) a
} deriving (Monad, Functor, Applicative)

getValue :: (HackcelError error field, Ord field) => field -> Eval field value error value
getValue f = Eval $
      do s <- get
         let EvalState { esHackcelState = HackcelState m _ } =  s
         case M.lookup f m of
           Nothing -> throwError (errorUnknownField f)
           Just (expr, Nothing) -> undefined -- TODO: Calculate expr and store the result in the state
           Just (expr, Just (FieldResult val _)) -> case val of
             Left e -> throwError e
             Right x -> return x

{-
evalExpression :: (HackcelError error field, Ord field) => field -> Eval field value error value
evalExpression f = Eval run
  where
    run s@(EvalState (HackcelState m) funcs) = case M.lookup f m of
      Nothing -> runEvalState (evalError (errorUnknownField f)) s



-}
    {-

and :: Expression String Bool MyError -> Expression String Bool MyError -> Eval String Bool MyError Bool
and ex ey = do
  x <- ex
  if x then
    ey
  else
    return False

(ExprEval . and) <$> pExp <*> pExp

and :: [Expression String Bool MyError] -> Eval String Bool MyError Bool
and [ex, ey] = do
  x <- ex
  if x then
    ey
  else
    return False

data Value = ValInt Int | ValString String
app :: String -> [Expression field value error] -> value
app "plus" [ex, ey] = do
  x <- ex
  y <- ey
  case x of
    ValInt x' -> case y of
      ValInt y' -> x' + y'
      _ -> ErrorType "Expected integer value as second argument of plus"
    ValString x' -> case y of
      ValString y' -> x' ++ y'
app "plus" _ = ErrorWrongArguments "plus"

-}
