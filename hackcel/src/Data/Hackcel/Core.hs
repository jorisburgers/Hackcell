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
  | ExprApp String [Expression field value error]

data EvalState field value error = EvalState
  { esHackcelState :: HackcelState field value error
  , esField :: field
  , esStack :: [field]
  }

newtype Eval field value error a = Eval {
  runEvalState :: E.ExceptT error (S.State (EvalState field value error)) a
} deriving (Monad, Functor, Applicative)

runEval :: Eval field value e a -> EvalState field value e -> (Either e a, EvalState field value e)
runEval = runState . runExceptT . runEvalState

getValue :: (HackcelError error field, Ord field) => field -> Eval field value error value
getValue f = Eval $
      do s <- get
         let EvalState { esHackcelState = HackcelState m _ } =  s
         case M.lookup f m of
           Nothing -> throwError (errorUnknownField f)
           Just (expr, Nothing) -> runEvalState $ evalExpression f
           Just (expr, Just (FieldResult val _)) -> case val of
             Left e -> throwError e
             Right x -> return x

evalExpression :: (HackcelError error field, Ord field) => field -> Eval field value error value
evalExpression f = Eval $
      do  s <- get
          let EvalState { esHackcelState = HackcelState m funcs } =  s
          case M.lookup f m of
            Nothing -> throwError (errorUnknownField f)
            Just (expr, _) -> do  tres <- runEvalState (evalExpr' expr)
                                  let res = Just $ FieldResult (Right tres) []
                                  let newm = insert f (expr,res) m
                                  put $ s {esHackcelState = HackcelState newm funcs}
                                  return tres
                              `catchError` (\e ->
                              do
                                  let res = Just $ FieldResult (Left e) []
                                  let newm = insert f (expr,res) m
                                  put $ s {esHackcelState = HackcelState newm funcs}
                                  throwError e)

  where
    evalExpr' :: (HackcelError error field, Ord field) => Expression field value error -> Eval field value error value
    evalExpr' (ExprLit val)  = return val
    evalExpr' (ExprField f2) = getValue f2
    evalExpr' (ExprApp f args) = Eval $ do  s <- get
                                            let EvalState { esHackcelState = HackcelState m funcs } =  s
                                            runEvalState $ funcs f args


-- newtype Test = Test { runTest :: E.ExceptT String (S.State (Double)) Int }
--
-- testing :: Test
-- testing = Test $
--           do s <- get
--              put (s + 1.0)
--              if s > 0 then
--                return 1
--                else do  let a = 3
--                         b <- runTest testing
--                         return b


    -- ExprField field
    -- | ExprLit value
    -- | ExprLetIn String (Expression field value error) (Expression field value error)
    -- | ExprApp String [Expression field value error]

--   where
--     run s@(EvalState (HackcelState m) funcs) = case M.lookup f m of
--       Nothing -> runEvalState (evalError (errorUnknownField f)) s
--
    {- data HackcelState field value error = HackcelState
      { fields :: Map field (Expression field value error, Maybe (FieldResult field value error))
      , app :: String -> [Expression field value error] -> Eval field value error value
      }



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
