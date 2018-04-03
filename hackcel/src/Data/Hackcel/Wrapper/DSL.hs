{-# language TupleSections #-}
module Data.Hackcel.Wrapper.DSL where

import Data.Hackcel.Core
import Data.Map.Lazy

(@@) ::  Expression field value error app -> field -> (field, Expression field value error app)
(@@) = flip (,)

getValues   :: (HackcelError error field, Ord field, Apply field value error app) 
            => [field] -> HackcelState field value error app
            -> ([Either error value], HackcelState field value error app)
getValues []     h = ([], h)
getValues (x:xs) h = (val : rest, h'')
  where
    (val, h') = runField x h
    (rest, h'') = getValues xs h'

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


