{-# language TupleSections #-}
module Data.Hackcel.Core.Utils where

import Data.Hackcel.Core.Spreadsheet
import Data.Hackcel.Core.Eval
import Data.Hackcel.Core.Expression
import Control.Monad.Except

import qualified Data.Map as M

-- | Initializes a spreadsheet.
createHackcel :: Spreadsheet field value error -- ^ The spreadsheet
              -> App field value error         -- ^ The handler function that handles function applications
              -> HackcelState field value error
createHackcel (Spreadsheet sheet) app = HackcelState (fmap (, Nothing) sheet) app

-- | Reports an error in the eval monad.
--   The error will be shown in the current field.
tError :: error -> Eval field value error return
tError =  Eval . throwError

-- | Returns the value of an argument if it is a value.
--   It reports an error if a range was passed instead of a value.
expectValue :: HackcelError error field => Argument field value error -> Eval field value error value
expectValue (AValue e)   = e
expectValue (ARange _ _) = tError errorExpectedValueGotRange

-- | Returns the range of an argument if it is a range.
--   It reports an error if a value was passed instead of a range.
expectRange :: HackcelError error field => Argument field value error -> Eval field value error (field, field)
expectRange (AValue _)   = tError errorExpectedRangeGotValue
expectRange (ARange a b) = return (a, b)

prettyprint :: (Show field, Show value, Show error) => HackcelState field value error -> String
prettyprint hackcel = foldr printField "" $ M.toAscList allFields
  where
    allFields = fields hackcel
    printField (k, a) s = show k ++ ": " ++ printValue a ++ "\n" ++ s
    printValue (expr, Nothing)  = "=" ++ show expr ++ ": <not calculated>"
    printValue (expr, Just val) = "=" ++ show expr ++ ": " ++ case fieldValue val of
      Left e -> show e
      Right v -> show v

prettyprinter :: (Show field, Show value, Show error) => HackcelState field value error -> IO ()
prettyprinter = putStrLn . prettyprint
