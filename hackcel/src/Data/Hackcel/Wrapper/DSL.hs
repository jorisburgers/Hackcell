module Data.Hackcel.Wrapper.DSL where

import Data.Hackcel.Core
import Data.Map.Lazy

(@@) ::  Expression field value error -> field -> (field, (Expression field value error, Maybe (FieldResult field value error)))
e @@ f =  (f,(e, Nothing))

constructSpreadSheet :: Map field (Expression field value error, Maybe (FieldResult field value error))
                        -> (String -> [Argument field value error] -> Eval field value error value)
                        -> EvalState field value error
constructSpreadSheet m hander = let hState = HackcelState {
                                        fields = m,
                                        app = hander
                                    } 
                                in
                                EvalState{
                                    esHackcelState = hState
                                  , esField = undefined
                                  , esStack = []
                                }

getValues   :: (HackcelError error field, Ord field) 
            => [field] -> EvalState field value error 
            -> ([Either error value], EvalState field value error)
getValues xs es    = valuesHelper xs es []
                    where 
                        valuesHelper    :: (HackcelError error field, Ord field) 
                                        => [field] 
                                        -> EvalState field value error 
                                        -> [Either error value]
                                        -> ([Either error value], EvalState field value error)
                        valuesHelper [] es before = (before, es)
                        valuesHelper (x:xs) es before = valuesHelper xs (snd res) (before ++ [fst res])
                                                where res = runEval (getValue x) es



