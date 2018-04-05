{-# language TupleSections #-}
-- | Provide a number of helper functions to easily write Expressions.
module Data.Hackcel.Wrapper.DSL(
        (@@)
    ,   getValues
    ,   FieldRange
    ,   getRange
    ,   TypeEq
    ,   typeEq
    ,   isUniformType

) where

import Data.Hackcel.Core
import Data.Map.Lazy

-- | Place the given expression in the given field
(@@) ::  Expression field value error app -> field -> (field, Expression field value error app)
(@@) = flip (,)

-- | Return a list where every `Either error value` corresponds to the given field
getValues   :: (HackcelError error field, Ord field, Apply field value error app)
            => [field] -> HackcelState field value error app
            -> ([Either error value], HackcelState field value error app)
getValues []     h = ([], h)
getValues (x:xs) h = (val : rest, h'')
  where
    (val, h') = runField x h
    (rest, h'') = getValues xs h'

-- | Describes a range of fields
class FieldRange field where
    -- | Returns, given two fields, all the fields in between these 2 fields
    getRange :: field -> field -> [field]
-- | Determines whether the types of two values are equal
class TypeEq value where
    -- | Given two values, return True if their types are identical, false otherwise
    typeEq :: value -> value -> Bool

-- | Determines if a list of value's all have the same type.
isUniformType :: (TypeEq value) => [value] -> Bool
isUniformType [] = True
isUniformType (x:xs) = all (x `typeEq`) xs
