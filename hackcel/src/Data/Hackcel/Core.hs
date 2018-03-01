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
  , app :: String -> [Argument field value error] -> Eval field value error value
  }

data Argument field value error
  = AValue (Eval field value error value)
  | ARange field field

data Parameter field value error  = PExpr (Expression field value error)
                                  | PRange field field

-- rangeApp "somals" [] [f1, f2] = 
data FieldResult field value error = FieldResult
  { fieldValue :: Either error value
  , fieldDependants :: [field]
  }

class HackcelError t field where
  errorUnknownField :: field -> t
  errorRecursion :: [field] -> t
  errorExpectedValueGotRange :: t
  errorExpectedRangeGotValue :: t

data Expression field value error
  = ExprField (field)
  | ExprLit value
  | ExprLetIn String (Expression field value error) (Expression field value error)
  | ExprApp String [Parameter field value error]

data EvalState field value error = EvalState
  { esHackcelState :: HackcelState field value error
  , esField :: field
  , esStack :: [field]
  }

newtype Eval field value error a = Eval {
  runEvalState :: E.ExceptT error (S.State (EvalState field value error)) a
} deriving (Monad, Functor, Applicative)

tError :: error -> Eval field value error return
tError =  Eval . throwError



runEval :: Eval field value e a -> EvalState field value e -> (Either e a, EvalState field value e)
runEval = runState . runExceptT . runEvalState

getValue :: (HackcelError error field, Ord field) => field -> Eval field value error value
getValue f = Eval $
      do s <- get
         let EvalState { esHackcelState = HackcelState m _ } =  s
         case M.lookup f m of
           Nothing -> throwError (errorUnknownField f)
           Just (expr, Nothing) -> runEvalState $ evalExpression f-- TODO: Calculate expr and store the result in the state
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
                                  let res = Just (FieldResult (Right tres) (referencedFields expr))
                                  let newm = insert f (expr,res) m
                                  put $ s {esHackcelState = HackcelState newm funcs}
                                  return tres
                                  -- TODO: Catch errors
  where
    evalExpr' :: (HackcelError error field, Ord field) => Expression field value error -> Eval field value error value
    evalExpr' (ExprLit val)  = return val
    evalExpr' (ExprField f2) = getValue f2
    evalExpr' (ExprApp f args) = Eval $ do  s <- get
                                            let EvalState { esHackcelState = HackcelState m funcs } =  s
                                            runEvalState $ funcs f (Prelude.map toArgument args)
    toArgument ::  (HackcelError error field, Ord field) => Parameter field value error -> Argument field value error
    toArgument (PRange f1 f2) = ARange f1 f2
    toArgument (PExpr e)      = AValue $ evalExpr' e


referencedFields :: Expression field value error -> [field]
referencedFields (ExprField f) = [f]
referencedFields (ExprLit _) = []
referencedFields (ExprLetIn _ e1 e2) = referencedFields e1 ++ referencedFields e2
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

expectValue :: HackcelError error field => Argument field value error -> Eval field value error value
expectValue (AValue e)   = e
expectValue (ARange _ _) = throwError errorExpectedValueGotRange

expectRange :: HackcelError error field => Argument field value error -> Eval field value error (field, field)
expectRange (AValue _)   = throwError errorExpectedRangeGotValue
expectRange (ARange a b) = return (a, b)

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
