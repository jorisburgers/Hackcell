{-# language MultiParamTypeClasses, FlexibleContexts, GeneralizedNewtypeDeriving, FunctionalDependencies #-}

module Data.Hackcel.Core.Eval (Eval(..), App, HackcelState(..), runField, Argument(..), getValue) where

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

runEval :: Eval field value e a -> EvalState field value e
        -> (Either e a, EvalState field value e)
runEval = runState . runExceptT . runEvalState

runField :: (HackcelError error field, Ord field) => field
        -> HackcelState field value error
        -> (Either error value, HackcelState field value error)
runField f hackcel = (result, finalHackcel)
  where
    initial = EvalState hackcel f []
    (result, EvalState finalHackcel _ _) = runEval (evalExpression f) initial

data Argument field value error
  = AValue (Eval field value error value)
  | ARange field field

getValue :: (HackcelError error field, Ord field) => field
         -> Eval field value error value
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

evalExpression :: (HackcelError error field, Ord field) => field
               -> Eval field value error value
evalExpression fld = Eval $
  do
    s <- get
    let EvalState { esHackcelState = HackcelState m funcs } =  s
    case M.lookup fld m of
      Nothing -> throwError (errorUnknownField fld)
      Just (expr, _) ->
        do
          -- Add field to the stack, throws error when already in stack
          runEvalState $ updateStack fld
          -- Get the result (might go into another evalExpression call)
          tres <- runEvalState (evalExpr' expr)
          -- Remove field from the stack again
          runEvalState popStack
          -- Update the state with the result
          runEvalState $ updateResult fld tres
          return tres
        `catchError` (\e ->
          do
            let res = Just $ FieldResult (Left e) []
            let newm = M.insert fld (expr,res) m
            put $ s {esHackcelState = HackcelState newm funcs}
            throwError e)
  where
    evalExpr' :: (HackcelError error field, Ord field)
              => Expression field value error -> Eval field value error value
    evalExpr' (ExprLit val)  = return val
    evalExpr' (ExprField f2) = getValue f2
    evalExpr' (ExprApp fld args) = Eval $ do
      s <- get
      let EvalState { esHackcelState = HackcelState m funcs } =  s
      runEvalState $ funcs fld (Prelude.map toArgument args)

    toArgument :: (HackcelError error field, Ord field)
               => Parameter field value error -> Argument field value error
    toArgument (PRange fld1 fld2) = ARange fld1 fld2
    toArgument (PExpr e)      = AValue $ evalExpr' e

    updateStack :: (HackcelError error field, Ord field) => field
                -> Eval field value error ()
    updateStack fld2 = Eval $
      do
        s <- get
        let EvalState { esStack = stack } =  s
        if fld2 `elem` stack
          then throwError (errorRecursion stack)
          else put $ s {esStack = fld2 : stack}

    popStack :: (HackcelError error field, Ord field)
             => Eval field value error ()
    popStack = Eval $
      do
        s <- get
        let EvalState { esStack = stack } =  s
        case stack of
          []       -> error "This should really not happen, the stack is empty when trying to pop"
          (_:news) -> put $ s {esStack = news}

updateResult :: (HackcelError error field, Ord field) => field
             -> value -> Eval field value error ()
updateResult fld val = Eval $
  do
    s <- get
    let EvalState { esHackcelState = HackcelState m funcs } =  s
    case M.lookup fld m of
      Nothing -> throwError (errorUnknownField fld)
      Just (expr, _) ->
        do
          let res = Just (FieldResult (Right val) (referencedFields expr))
          let newm = M.insert fld (expr, res) m
          put $ s {esHackcelState = HackcelState newm funcs}


referencedFields :: Expression field value error -> [field]
referencedFields (ExprField f) = [f]
referencedFields (ExprLit _) = []
referencedFields (ExprApp _ _) = []

dependencies :: (HackcelError error field, Ord field) => field
             -> Eval field value error [field]
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
