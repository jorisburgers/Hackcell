{-# language FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Data.Hackcel.Wrapper.Numbers where

import Data.Hackcel.Core
import Data.Hackcel.Wrapper.DSL
import Data.Foldable hiding (sum)

import Control.Monad.Except
import Data.Maybe
import Prelude hiding (sum)

import Debug.Trace

data Value  = ValInt Int
            | ValDouble Double
            | ValBool Bool
            deriving (Show, Eq)

data NumberError    = RecursionError String
                    | UnknownFieldError String
                    | DivideByZeroError String
                    | ErrorUnexpectedValue
                    | ErrorUnexpectedRange
                    | ErrorInvalidType
                    deriving (Show, Eq)

instance TypeEq Value where
    typeEq (ValInt _)       (ValInt _)      = True
    typeEq (ValDouble _)    (ValDouble _)   = True
    typeEq (ValBool _)      (ValBool _)     = True
    typeEq _ _ = False

type Eval' field = Eval field Value NumberError Value

valueDouble :: Double -> Expression field Value NumberError
valueDouble = ExprLit . ValDouble

fromValueDouble :: Value -> Double
fromValueDouble (ValDouble x)   = x
fromValueDouble _               = error "Value is not a Double"

valueInt :: Int -> Expression field Value NumberError
valueInt = ExprLit . ValInt

fromValueInt :: Value -> Int
fromValueInt (ValInt x) = x
fromValueInt _          = error "Value is not an Int"


intOpHandler :: (Int -> Int -> Eval field Value NumberError Int) -> [Value] -> Eval field Value NumberError Value
intOpHandler op [ValInt x, ValInt y] = fmap ValInt (op x y)

doubleOpHandler :: (Double -> Double -> Eval field Value NumberError Double) -> [Value] -> Eval field Value NumberError Value
doubleOpHandler op [ValDouble x, ValDouble y] = fmap ValDouble (op x y)


intNameOp :: [(String, Int -> Int -> Eval field Value NumberError Int)]
intNameOp  = [
    ("plus", \x y -> return (x + y)),
    ("minus", \x y -> return (x - y)),
    ("times", \x y -> return (x * y)),
    ("divide", intDivision)
    ]

doubleNameOp :: [(String, Double -> Double -> Eval field Value NumberError Double)]
doubleNameOp  = [
    ("plus", \x y -> return (x + y)),
    ("minus", \x y -> return (x - y)),
    ("times", \x y -> return (x * y)),
    ("divide", doubleDivision)
    ]



intDivision :: Int -> Int -> Eval field Value NumberError Int
intDivision x y |  y == 0 = tError $ DivideByZeroError "div 0" -- throw error
                |  otherwise = return $ x `div` y

doubleDivision :: Double -> Double -> Eval field Value NumberError Double
doubleDivision x y  |  y == 0 = tError $ DivideByZeroError "div 0"-- throw error
                    |  otherwise = return $ x / y

compareOperator :: Ord a => (a -> a -> Bool) -> a -> a -> Value
compareOperator op x y  = (ValBool (x `op` y))

numberHandler :: (FieldRange field, Ord field, HackcelError NumberError field) 
                => String 
                -> [Argument field Value NumberError] 
                -> Eval field Value NumberError Value
numberHandler "sum" p   =  do 
                        (f1, f2) <- expectRange (head p)
                        x <- getValue f1
                        case x of
                            ValInt _    -> sumInt f1 f2
                            ValDouble _ -> sumDouble f1 f2
                            _           -> tError ErrorInvalidType
numberHandler "eq"  args   = do
                            x <- expectValue (head args)
                            y <- expectValue (args !! 1)
                            return (ValBool $ x == y)
numberHandler "not" args = do
                            x <- expectValue (head args)
                            case x of
                                ValBool x -> return $ ValBool (not x)
                                _ -> tError ErrorInvalidType
numberHandler "lt" args = do
                            x <- expectValue (head args)
                            y <- expectValue (args !! 1)
                            let xy = (x, y)
                            case xy of
                                (ValInt x', ValInt y') -> return (ValBool $ x' <= y')
                                (ValDouble x', ValDouble y') -> return (ValBool $ x' <= y')
                                _ -> tError ErrorInvalidType
numberHandler "gt" args =   do
                            x <- expectValue (head args)
                            y <- expectValue (args !! 1)
                            let xy = (x, y)
                            case xy of
                                (ValInt x', ValInt y') -> return (ValBool $ x' >= y')
                                (ValDouble x', ValDouble y') -> return (ValBool $ x' <= y')
                                _ -> tError ErrorInvalidType
numberHandler "ge"  args =  do
                                ValBool gt <- numberHandler "gt" args
                                ValBool eq <- numberHandler "eq" args
                                return (ValBool $ gt || eq)
numberHandler "le"  args =  do
                                ValBool lt <- numberHandler "lt" args
                                ValBool eq <- numberHandler "eq" args
                                return (ValBool $ lt || eq)
numberHandler "if"  p   = do
                            cond <- expectValue (head p)
                            tBranch <- expectValue (p !! 1)
                            fBranch <- expectValue (p !! 2)
                            if cond == ValBool True then
                                return tBranch
                            else
                                return fBranch
numberHandler name  p   =  do
                            x <- expectValue (head p)
                            y <- expectValue (p !! 1)
                            case x of
                                ValInt _    -> intOpHandler (op intNameOp) [x, y]
                                ValDouble _ -> doubleOpHandler (op doubleNameOp) [x, y]
                                ValBool _   -> booleanHandler name [x, y]
                            where op lst = fromJust $ lookup name lst

rangeHandler :: (FieldRange field, Ord field, HackcelError NumberError field) => ([Value] -> Value) -> field -> field -> Eval field Value NumberError Value
rangeHandler f f1 f2 = do
                        values <- mapM getValue (getRange f1 f2)
                        if not $ isUniformType $ traceShow values values then
                            tError $ ErrorInvalidType
                        else 
                            return $ f values
                           

sumInt :: (FieldRange field, Ord field, HackcelError NumberError field) => field -> field -> Eval field Value NumberError Value 
sumInt f1 f2 = rangeHandler s f1 f2
                where
                    s values = ValInt $ foldr (+) 0 $ map fromInt values
                    fromInt (ValInt x) = x

sumDouble :: (FieldRange field, Ord field, HackcelError NumberError field) => field -> field -> Eval field Value NumberError Value 
sumDouble f1 f2 = rangeHandler s f1 f2
                where
                    s values = ValDouble $ foldr (+) 0 $ map fromInt values
                    fromInt (ValDouble x) = x

booleanHandler ::   String 
                ->  [Value]
                ->  Eval field Value NumberError Value
booleanHandler "and" [(ValBool b1), (ValBool b2)]   = return (ValBool $ b1 && b2)
booleanHandler "or" [(ValBool b1), (ValBool b2)]    = return (ValBool $ b1 || b2)