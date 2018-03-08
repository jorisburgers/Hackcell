{-# language MultiParamTypeClasses, FlexibleContexts, GeneralizedNewtypeDeriving, FunctionalDependencies #-}

module Data.Hackcel.Core.Expression (Parameter(..), HackcelError(..), Expression(..)) where

import Data.List (intercalate)

data Parameter field value error
  = PExpr (Expression field value error)
  | PRange field field

instance (Show field, Show value, Show error) => Show (Parameter field value error)
  where
    show (PExpr expr) = show expr
    show (PRange f1 f2) = "[" ++ show f1 ++ ":" ++ show f2 ++ "]"

class HackcelError t field | t -> field where
  errorUnknownField :: field -> t
  errorRecursion :: [field] -> t
  errorExpectedValueGotRange :: t
  errorExpectedRangeGotValue :: t

data Expression field value error
  = ExprField field
  | ExprLit value
  | ExprApp String [Parameter field value error]

instance (Show field, Show value, Show error) => Show (Expression field value error)
  where
    show (ExprField field) = "$" ++ show field
    show (ExprLit value)   = show value
    show (ExprApp name ps) = name ++ "(" ++ intercalate "," (map show ps) ++ ")"
