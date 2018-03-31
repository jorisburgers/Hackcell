{-# language MultiParamTypeClasses, FlexibleContexts, GeneralizedNewtypeDeriving #-}

module Data.Hackcel.Core.Eval (Eval(..), App, HackcelState(..), EvalState(..), runField
                              , Argument(..), getValue, runEval
                              , set) where

import Control.Monad
import qualified Data.Map.Strict as M
import Control.Monad.Except
import Control.Monad.State.Lazy

import Data.Hackcel.Core.Spreadsheet
import Data.Hackcel.Core.Expression

import Data.List (intercalate)

-- | A functions that handles function applications in expressions of the spreadsheet.
type App field value error = String -> [Argument field value error] -> Eval field value error value

type FieldsMap field value error = M.Map field (Expression field value error, Maybe (FieldResult field value error))

-- | The state of a Hackcel spreadsheet.
data HackcelState field value error = HackcelState
  { fields :: FieldsMap field value error
  -- ^ The fields of the spreadsheet, with their value or error and their dependencies.
  , app :: App field value error
  -- ^ The handler for function applications
  }

-- | The state of an evaluation of a field.
data EvalState field value error = EvalState
  { esHackcelState :: HackcelState field value error
  , esField :: field
  -- ^ The current field that is being calculated.
  , esStack :: [field]
  -- ^ The stack of fields of the current calculations. Used to detect cycles.
  }

-- | The Eval monad is used to evaluate a field. It keeps track of errors that occur during the calculation,
--   feeds the dependency tracking and it can detect cyclic references.
newtype Eval field value error a = Eval {
  runEvalState :: ExceptT error (State (EvalState field value error)) a
} deriving (Monad, Functor, Applicative)

-- | Runs a calculation in the Eval monad.
runEval :: Eval field value e a -> EvalState field value e
        -> (Either e a, EvalState field value e)
runEval = runState . runExceptT . runEvalState

-- | Given a HackcelState and field, calculates the value of the specified field.
--   Results are memoized and stored in the returned HackcelState.
runField :: (HackcelError error field, Ord field) => field
        -> HackcelState field value error
        -> (Either error value, HackcelState field value error)
runField f hackcel = (x, finalHackcel)
  where
    initial = EvalState hackcel f []
    (x, EvalState finalHackcel _ _) = runEval (fromFieldResult $ evalExpression [] f) initial

set :: (Ord field)
    => HackcelState field value error
    -> field
    -> Expression field value error -> HackcelState field value error
set s f e = s {fields = fields''}
  where
    -- replaces field, if it is allready present.
    fields' = M.insert f (e, Nothing) (fields s)
    fields'' = case M.lookup f (fields s) of
      -- Invalidate the dependants of this field
      Just (_, Just (FieldResult _ dependants)) -> foldr (flip invalidate) fields' dependants
      -- New field, or existing field that was not calculated yet
      Nothing -> fields'

invalidate :: (Ord field)
           => FieldsMap field value error
           -> field
           -> FieldsMap field value error
invalidate fields f = case M.lookup f fields of
  Just (expr, Just (FieldResult _ dependants)) ->
    let fields' = M.insert f (expr, Nothing) fields
    in foldr (flip invalidate) fields' dependants
  _ -> fields -- Field was already invalidated or removed, so we do not need to invalidate anymore

-- | Represents an argument of a function application. Can either be a normal value
--   or a range.
data Argument field value error
  = AValue (Eval field value error value)
  | ARange field field

-- | Finds the value of a field.
--   Returns a memoized value if possible.
getValue :: (HackcelError error field, Ord field) => field
         -> Eval field value error value
getValue f = Eval $
  do
    state@EvalState { esHackcelState = HackcelState m app, esField = currentField } <- get
    case M.lookup f m of
      Nothing -> throwError (errorUnknownField f)
      Just (expr, memoized) -> do
        FieldResult val dependants <- maybe (runEvalState $ evalExpression [] f) return memoized
        let dependants' = if currentField `elem` dependants then dependants else currentField : dependants
        let m' = M.insert f (expr, Just (FieldResult val dependants')) m
        put state{ esHackcelState = HackcelState m' app }
        case val of
          Left e -> throwError e
          Right x -> return x
      {-
      Just (expr, Nothing) -> runEvalState $ evalExpression f
      Just (expr, Just (FieldResult val dependants)) -> case val of
        Left e -> throwError e
        Right x -> return x -}

-- | Evaluates the expression of some field
evalExpression :: (HackcelError error field, Ord field)
               => [field]
               -> field
               -> Eval field value error (FieldResult field value error)
evalExpression dependants fld = Eval $
  do
    s <- get
    let EvalState { esHackcelState = HackcelState m funcs } =  s
    case M.lookup fld m of
      Nothing -> throwError (errorUnknownField fld)
      Just (expr, _) ->
        do
          -- Add field to the stack, throws error when already in stack
          runEvalState $ pushStack fld
          -- Get the result (might go into another evalExpression call)
          tres <- runEvalState (evalExpr' expr)
          -- Remove field from the stack again
          runEvalState popStack
          -- Update the state with the result
          runEvalState $ updateResult dependants fld tres
        `catchError` (\e -> do
          let res = FieldResult (Left e) dependants
          let newm = M.insert fld (expr, Just res) m
          put $ s {esHackcelState = HackcelState newm funcs}
          return res)
  where
    evalExpr' :: (HackcelError error field, Ord field)
              => Expression field value error -> Eval field value error value
    evalExpr' (ExprLit val)  = return val
    evalExpr' (ExprField f2) = getValue f2
    evalExpr' (ExprApp fld args) = Eval $ do
      s <- get
      let EvalState { esHackcelState = HackcelState m funcs } =  s
      runEvalState $ funcs fld (map toArgument args)

    toArgument :: (HackcelError error field, Ord field)
               => Parameter field value error -> Argument field value error
    toArgument (PRange fld1 fld2) = ARange fld1 fld2
    toArgument (PExpr e)      = AValue $ evalExpr' e

    pushStack :: (HackcelError error field, Ord field) => field
                -> Eval field value error ()
    pushStack fld2 = Eval $
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

updateResult :: (HackcelError error field, Ord field)
             => [field]
             -> field
             -> value -> Eval field value error (FieldResult field value error)
updateResult dependants fld val = Eval $
  do
    s@EvalState { esHackcelState = HackcelState m funcs } <- get
    case M.lookup fld m of
      Nothing -> error "The impossible happened: updateResult should not be called with a field that does not exist"
      Just (expr, _) ->
        do
          let res = FieldResult (Right val) dependants
          let newm = M.insert fld (expr, Just res) m
          put $ s {esHackcelState = HackcelState newm funcs}
          return res


catchErrorEval :: Eval field value error a -> (error -> Eval field value error a) -> Eval field value error a
catchErrorEval try catch = Eval $ runEvalState try `catchError` (runEvalState . catch)

toFieldResult :: [field] -> Eval field value error value -> Eval field value error (FieldResult field value error)
toFieldResult dependants e = (e >>= (\x -> return $ FieldResult (Right x) dependants)) `catchErrorEval` (\err -> return $ FieldResult (Left err) dependants)

fromFieldResult :: Eval field value error (FieldResult field value error) -> Eval field value error value
fromFieldResult e = do
  FieldResult x _ <- e
  case x of
    Left err -> Eval $ throwError err
    Right value -> return value
    
{- If we want to embed the IO monad, we need underlying code. Up for discussion.
-- | The Eval monad is used to evaluate a field. It keeps track of errors that occur during the calculation,
--   feeds the dependency tracking and it can detect cyclic references.
newtype Eval field value error a = Eval {
  runEvalState :: ExceptT error (StateT (EvalState field value error) IO) a
} deriving (Monad, Functor, Applicative)

-- | Runs a calculation in the Eval monad.
runEval :: Eval field value e a -> EvalState field value e
         -> IO (Either e a, EvalState field value e)
runEval = runStateT . runExceptT . runEvalState

-- | Given a HackcelState and field, calculates the value of the specified field.
--   Results are memoized and stored in the returned HackcelState.
runField :: (HackcelError error field, Ord field) => field
        -> HackcelState field value error
        -> IO (Either error value, HackcelState field value error)
runField f hackcel = do (r, EvalState finalHackcel _ _) <- runEval (evalExpression f) initial
                        return (r, finalHackcel)
  where
    initial = EvalState hackcel f []
-}