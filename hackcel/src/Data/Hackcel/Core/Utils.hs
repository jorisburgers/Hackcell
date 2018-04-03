{-# language TupleSections #-}
module Data.Hackcel.Core.Utils where

import Data.Hackcel.Core.Spreadsheet
import Data.Hackcel.Core.Eval
import Data.Hackcel.Core.Expression
import Control.Monad.Except

import qualified Data.Map as M

-- | Initializes a spreadsheet.
createHackcel :: Apply field value error app
              => Spreadsheet field value error app -- ^ The spreadsheet
              -> HackcelState field value error app
createHackcel (Spreadsheet sheet) = HackcelState (fmap (, Nothing) sheet)

-- | Reports an error in the eval monad.
--   The error will be shown in the current field.
tError :: error -> Eval field value error app return
tError =  Eval . throwError

-- | Returns the value of an argument if it is a value.
--   It reports an error if a range was passed instead of a value.
expectValue :: HackcelError error field => Argument field value error app -> Eval field value error app value
expectValue (AValue e)   = e
expectValue (ARange _ _) = tError errorExpectedValueGotRange

-- | Returns the range of an argument if it is a range.
--   It reports an error if a value was passed instead of a range.
expectRange :: HackcelError error field => Argument field value error app -> Eval field value error app (field, field)
expectRange (AValue _)   = tError errorExpectedRangeGotValue
expectRange (ARange a b) = return (a, b)

prettyprint :: (Show field, Show value, Show error, Show app) => HackcelState field value error app -> String
prettyprint hackcel = foldr printField "" $ M.toAscList allFields
  where
    allFields = fields hackcel
    printField (k, a) s = show k ++ ": " ++ printValue a ++ "\n" ++ s
    printValue (expr, Nothing)  = "=" ++ show expr ++ ": <not calculated>"
    printValue (expr, Just val) = "=" ++ show expr ++ ": " ++ case fieldValue val of
      Left e -> show e
      Right v -> show v

prettyprinter :: (Show field, Show value, Show error, Show app) => HackcelState field value error app -> IO ()
prettyprinter = putStrLn . prettyprint

-- | Very ugly function, that calculates all results of all the expressions
evalAll :: (HackcelError error field, Ord field, Apply field value error app)
        => HackcelState field value error app
        -> HackcelState field value error app
evalAll s = case allFields of
  []  -> s
  f:_ -> helper f allFields
  where
    allFields = M.keys $ fields s
    initial f = EvalState s f []

    getAll :: (HackcelError error field, Ord field, Apply field value error app) => [field] -> Eval field value error app ()
    getAll fs = Eval $ mapM_ (runEvalState.getValue') fs

    -- To make sure the calculations continues, even if one field has an error.
    getValue' f = Eval $ do (runEvalState.getValue) f; return ()
                         `catchError` \_ -> return ()

    helper f fs = finalHackcel
      where
        (_, EvalState finalHackcel _ _) = runEval (getAll fs) (initial f)
