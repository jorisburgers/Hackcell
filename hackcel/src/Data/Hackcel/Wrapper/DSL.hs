{-# language TupleSections #-}
module Data.Hackcel.Wrapper.DSL where

import Data.Hackcel.Core
import Data.Map.Lazy

(@@) ::  Expression field value error -> field -> (field, Expression field value error)
(@@) = flip (,)

getValues   :: (HackcelError error field, Ord field) 
            => [field] -> HackcelState field value error 
            -> ([Either error value], HackcelState field value error)
getValues []     h = ([], h)
getValues (x:xs) h = (val : rest, h'')
  where
    (val, h') = runField x h
    (rest, h'') = getValues xs h'

class FieldRange field where
    getRange :: field -> field -> [field]

class TypeEq value where
    typeEq :: value -> value -> Bool

isUniformType :: (TypeEq value) => [value] -> Bool
isUniformType [] = True
isUniformType (x:xs) = all (x `typeEq`) xs

{-= valuesHelper xs es []
                    where 
                        valuesHelper    :: (HackcelError error field, Ord field) 
                                        => [field] 
                                        -> EvalState field value error 
                                        -> [Either error value]
                                        -> ([Either error value], EvalState field value error)
                        valuesHelper [] es before = (before, es)
                        valuesHelper (x:xs) es before = valuesHelper xs (snd res) (before ++ [fst res])
                                                where res = runEval (getValue x) es
-}


