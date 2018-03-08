{-# language MultiParamTypeClasses, FlexibleContexts, GeneralizedNewtypeDeriving, FunctionalDependencies #-}

module Data.Hackcel.Core.Eval (Eval(..), App, HackcelState(..), runEval, Argument(..), getValue) where

import Control.Monad
import qualified Data.Map.Strict as M
import Control.Monad.Except
import Control.Monad.State.Lazy

import Data.Hackcel.Core.Spreadsheet
import Data.Hackcel.Core.Expression

import Data.List (intercalate)

type App field value error = String -> [Argument field value error] -> Eval field value error value
data HackcelState field value error = HackcelState
  { fields :: M.Map field (Expression field value error, Maybe (FieldResult field value error))
  , app :: App field value error
  }

data EvalState field value error = EvalState
  { esHackcelState :: HackcelState field value error
  , esField :: field
  , esStack :: [field]
  }

newtype Eval field value error a = Eval {
  runEvalState :: ExceptT error (State (EvalState field value error)) a
} deriving (Monad, Functor, Applicative)

runEval :: Eval field value e a -> EvalState field value e -> (Either e a, EvalState field value e)
runEval = runState . runExceptT . runEvalState

data Argument field value error
  = AValue (Eval field value error value)
  | ARange field field

getValue :: (HackcelError error field, Ord field) => field -> Eval field value error value
getValue f = Eval $
  do
    s <- get
    let EvalState { esHackcelState = HackcelState m _ } =  s
    case M.lookup f m of
      Nothing -> throwError (errorUnknownField f)
      Just (expr, Nothing) -> runEvalState $ evalExpression f
      Just (expr, Just (FieldResult val _)) -> case val of
        Left e -> throwError e
        Right x -> return x

evalExpression :: (HackcelError error field, Ord field) => field -> Eval field value error value
evalExpression f = Eval $
  do
    s <- get
    let EvalState { esHackcelState = HackcelState m funcs } =  s
    case M.lookup f m of
      Nothing -> throwError (errorUnknownField f)
      Just (expr, _) ->
        do
          tres <- runEvalState (evalExpr' expr)
          let res = Just (FieldResult (Right tres) (referencedFields expr))
          let newm = M.insert f (expr,res) m
          put $ s {esHackcelState = HackcelState newm funcs}
          return tres
        `catchError` (\e ->
          do
            let res = Just $ FieldResult (Left e) []
            let newm = M.insert f (expr,res) m
            put $ s {esHackcelState = HackcelState newm funcs}
            throwError e)
  where
    evalExpr' :: (HackcelError error field, Ord field) => Expression field value error -> Eval field value error value
    evalExpr' (ExprLit val)  = return val
    evalExpr' (ExprField f2) = getValue f2
    evalExpr' (ExprApp f args) = Eval $ do
      s <- get
      let EvalState { esHackcelState = HackcelState m funcs } =  s
      runEvalState $ funcs f (Prelude.map toArgument args)
    toArgument ::  (HackcelError error field, Ord field) => Parameter field value error -> Argument field value error
    toArgument (PRange f1 f2) = ARange f1 f2
    toArgument (PExpr e)      = AValue $ evalExpr' e

referencedFields :: Expression field value error -> [field]
referencedFields (ExprField f) = [f]
referencedFields (ExprLit _) = []
referencedFields (ExprApp _ _) = []

dependencies :: (HackcelError error field, Ord field) => field -> Eval field value error [field]
dependencies field = Eval $ do
          s <- get
          let EvalState { esHackcelState = HackcelState m funcs } =  s
          case M.lookup field m of
            Nothing -> throwError (errorUnknownField field)
            Just (expr, Nothing) -> return []
            Just (expr, Just fr) ->
                        do
                        let FieldResult { fieldDependants = depends } = fr
                        return depends
  -- TODO: Do ExprLetIn

