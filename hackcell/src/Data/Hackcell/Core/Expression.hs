{-# language MultiParamTypeClasses, FlexibleContexts, GeneralizedNewtypeDeriving, FunctionalDependencies #-}
-- | Provides the functionality for creating Expressions, Parameters and Errors
module Data.Hackcell.Core.Expression (Parameter(..), HackcellError(..), Expression(..)) where

import Data.List (intercalate)

-- | Represents a parameter of a function application.
--   A parameter can either be an expression or a range.
data Parameter field value error app
  = PExpr (Expression field value error app)
  | PRange field field

instance (Show field, Show value, Show error, Show app) => Show (Parameter field value error app)
  where
    show (PExpr expr) = show expr
    show (PRange f1 f2) = show f1 ++ ";" ++ show f2

-- | An error type should at least provide the error messages
--   that are member of this type class.
class HackcellError t field | t -> field where
  -- | Unknown field error is thrown when an expression
  --   refers to a field that does not exist.
  errorUnknownField :: field -> t
  -- | Recursion error is thrown when some field (in)directly refers
  --   to itself. The cycle of this recursion is passed as an argument.
  errorRecursion :: [field] -> t
  -- | This error is thrown when some function in an expression expected a value,
  --   but got a range instead.
  errorExpectedValueGotRange :: t
  -- | This error is thrown when some function in an expression expected a range,
  --   but got a value instead.
  errorExpectedRangeGotValue :: t

-- | Represents an expression that is in a cell
data Expression field value error app
  = ExprField field  -- ^ References another field
  | ExprLit value   -- ^ Represents a constant value
  | ExprApp app [Parameter field value error app] -- ^ Represents an operatorion with parameters

instance (Show field, Show value, Show error, Show app) => Show (Expression field value error app) where
  show (ExprField field) = show field
  show (ExprLit value)   = show value
  show (ExprApp name ps) = show name ++ "(" ++ intercalate "," (map show ps) ++ ")"
