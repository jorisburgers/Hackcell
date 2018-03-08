{-# language MultiParamTypeClasses, FlexibleContexts, GeneralizedNewtypeDeriving, FunctionalDependencies, TupleSections #-}

module Data.Hackcel.Core.Spreadsheet(Spreadsheet(..), FieldResult(..)) where

import Control.Monad
import qualified Data.Map.Strict as M
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Hackcel.Core.Expression

import Data.List (intercalate)

newtype Spreadsheet field value error = Spreadsheet { unSpreadsheet :: M.Map field (Expression field value error) }

data FieldResult field value error = FieldResult
  { fieldValue :: Either error value
  , fieldDependants :: [field]
  }
