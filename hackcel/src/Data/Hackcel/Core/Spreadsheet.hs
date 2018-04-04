{-# language MultiParamTypeClasses, FlexibleContexts, GeneralizedNewtypeDeriving, FunctionalDependencies, TupleSections #-}

module Data.Hackcel.Core.Spreadsheet(Spreadsheet(..), FieldResult(..)) where

import Control.Monad
import qualified Data.Map.Strict as M
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Hackcel.Core.Expression

import Data.List (intercalate)

-- | A spreadsheet is a map from fields to expressions
newtype Spreadsheet field value error app = Spreadsheet { unSpreadsheet :: M.Map field (Expression field value error app) }

-- | Results of the calculation of a field is stored as a FieldResult.
--   It contains either the value or the error and its dependencies.
data FieldResult field value error = FieldResult
  { fieldValue :: Either error value
  , fieldDependants :: [field]
  }
