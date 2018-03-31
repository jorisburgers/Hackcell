{-# language MultiParamTypeClasses, FlexibleContexts, GeneralizedNewtypeDeriving #-}

module Data.Hackcel.Core.Eval (Eval(..), Apply, HackcelState(..), EvalState(..), runField
                              , Argument(..), getValue, runEval
                              , insertExpression) where

import Control.Monad
import qualified Data.Map.Strict as M
import Control.Monad.Except
import Control.Monad.State.Lazy

import Data.Hackcel.Core.Spreadsheet
import Data.Hackcel.Core.Expression

import Data.List (intercalate)

-- | Handles function applications in expressions of the spreadsheet.
class Apply field value error app where
  apply :: app -> [Argument field value error a] -> Eval field value error a value

-- | The state of a Hackcel spreadsheet.
data HackcelState field value error app = HackcelState
  { fields :: M.Map field (Expression field value error app, Maybe (FieldResult field value error))
  -- ^ The fields of the spreadsheet, with their value or error and their dependencies.
  }

-- | The state of an evaluation of a field.
data EvalState field value error app = EvalState
  { esHackcelState :: HackcelState field value error app
  , esField :: field
  -- ^ The current field that is being calculated.
  , esStack :: [field]
  -- ^ The stack of fields of the current calculations. Used to detect cycles.
  }

-- | The Eval monad is used to evaluate a field. It keeps track of errors that occur during the calculation,
--   feeds the dependency tracking and it can detect cyclic references.
newtype Eval field value error app a = Eval {
  runEvalState :: ExceptT error (State (EvalState field value error app)) a
} deriving (Monad, Functor, Applicative)

-- | Runs a calculation in the Eval monad.
runEval :: Eval field value e app a -> EvalState field value e app
        -> (Either e a, EvalState field value e app)
runEval = runState . runExceptT . runEvalState

-- | Given a HackcelState and field, calculates the value of the specified field.
--   Results are memoized and stored in the returned HackcelState.
runField :: (HackcelError error field, Ord field, Apply field value error app)
        => field
        -> HackcelState field value error app
        -> (Either error value, HackcelState field value error app)
runField f hackcel = (x, finalHackcel)
  where
    initial = EvalState hackcel f []
    (x, EvalState finalHackcel _ _) = runEval (fromFieldResult $ evalExpression [] f) initial

insertExpression :: (HackcelError error field, Ord field) => HackcelState field value error app
                 -> field -> Expression field value error app -> HackcelState field value error app
insertExpression s f e = s {fields = newmap}
  where
    oldmap = fields s
    -- replaces field, if it is allready present.
    newmap = M.insert f (e, Nothing) oldmap

-- | Represents an argument of a function application. Can either be a normal value
--   or a range.
data Argument field value error app
  = AValue (Eval field value error app value)
  | ARange field field

-- | Finds the value of a field.
--   Returns a memoized value if possible.
getValue :: (HackcelError error field, Ord field, Apply field value error app)
         => field
         -> Eval field value error app value
getValue f = Eval $
  do
    state@EvalState { esHackcelState = HackcelState m, esField = currentField } <- get
    case M.lookup f m of
      Nothing -> throwError (errorUnknownField f)
      Just (expr, memoized) -> do
        FieldResult val dependants <- maybe (runEvalState $ evalExpression [] f) return memoized
        let dependants' = if currentField `elem` dependants then dependants else currentField : dependants
        let m' = M.insert f (expr, Just (FieldResult val dependants')) m
        put state{ esHackcelState = HackcelState m' }
        case val of
          Left e -> throwError e
          Right x -> return x
      {-
      Just (expr, Nothing) -> runEvalState $ evalExpression f
      Just (expr, Just (FieldResult val dependants)) -> case val of
        Left e -> throwError e
        Right x -> return x -}

-- | Evaluates the expression of some field
evalExpression :: (HackcelError error field, Ord field, Apply field value error app)
               => [field]
               -> field
               -> Eval field value error app (FieldResult field value error)
evalExpression dependants fld = Eval $
  do
    s <- get
    let EvalState { esHackcelState = HackcelState m } =  s
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
          put $ s {esHackcelState = HackcelState newm}
          return res)
  where
    evalExpr' :: (HackcelError error field, Ord field, Apply field value error app)
              => Expression field value error app -> Eval field value error app value
    evalExpr' (ExprLit val)  = return val
    evalExpr' (ExprField f2) = getValue f2
    evalExpr' (ExprApp fld args) = Eval $ do
      s <- get
      let EvalState { esHackcelState = HackcelState m } =  s
      runEvalState $ apply fld (map toArgument args)

    toArgument :: (HackcelError error field, Ord field, Apply field value error app)
               => Parameter field value error app -> Argument field value error app
    toArgument (PRange fld1 fld2) = ARange fld1 fld2
    toArgument (PExpr e)      = AValue $ evalExpr' e

    pushStack :: (HackcelError error field, Ord field) => field
                -> Eval field value error app ()
    pushStack fld2 = Eval $
      do
        s <- get
        let EvalState { esStack = stack } =  s
        if fld2 `elem` stack
          then throwError (errorRecursion stack)
          else put $ s {esStack = fld2 : stack}

    popStack :: (HackcelError error field, Ord field)
             => Eval field value error app ()
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
             -> value -> Eval field value error app (FieldResult field value error)
updateResult dependants fld val = Eval $
  do
    s@EvalState { esHackcelState = HackcelState m } <- get
    case M.lookup fld m of
      Nothing -> error "The impossible happened: updateResult should not be called with a field that does not exist"
      Just (expr, _) ->
        do
          let res = FieldResult (Right val) dependants
          let newm = M.insert fld (expr, Just res) m
          put $ s {esHackcelState = HackcelState newm}
          return res

catchErrorEval :: Eval field value error app a -> (error -> Eval field value error app a) -> Eval field value error app a
catchErrorEval try catch = Eval $ runEvalState try `catchError` (runEvalState . catch)

toFieldResult :: [field] -> Eval field value error app value -> Eval field value error app (FieldResult field value error)
toFieldResult dependants e = (e >>= (\x -> return $ FieldResult (Right x) dependants)) `catchErrorEval` (\err -> return $ FieldResult (Left err) dependants)

fromFieldResult :: Eval field value error app (FieldResult field value error) -> Eval field value error app value
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