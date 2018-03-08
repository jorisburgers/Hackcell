{-# language TupleSections #-}
module Data.Hackcel.Core.Utils where

import Data.Hackcel.Core.Spreadsheet
import Data.Hackcel.Core.Eval
import Data.Hackcel.Core.Expression
import Control.Monad.Except

import qualified Data.Map as M

createHackcel :: Spreadsheet field value error -> App field value error -> HackcelState field value error
createHackcel (Spreadsheet sheet) app = HackcelState (fmap (, Nothing) sheet) app

tError :: error -> Eval field value error return
tError =  Eval . throwError

expectValue :: HackcelError error field => Argument field value error -> Eval field value error value
expectValue (AValue e)   = e
expectValue (ARange _ _) = tError errorExpectedValueGotRange

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
