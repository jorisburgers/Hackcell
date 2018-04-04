{-# language FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Data.Hackcel.Wrapper.Numbers where

import Data.Hackcel.Core
import Data.Hackcel.Wrapper.DSL
import Data.Foldable hiding (sum)

import Control.Monad.Except
import Data.Maybe
import Prelude hiding (sum, LT, GT, EQ)

import Debug.Trace

data Value  = ValInt Int
            | ValDouble Double
            | ValBool Bool
            deriving (Eq)

instance Show Value where
  show (ValInt x)    = show x
  show (ValDouble x) = show x
  show (ValBool x)   = show x

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

type Eval' field = Eval field Value NumberError Fns

data Fns
    = Plus
    | Minus
    | Times
    | Divide
    | Sum
    | If
    | LT
    | LE
    | GT
    | GE
    | Not
    | EQ
    | And
    | Or
    deriving (Eq, Show, Ord)

op :: Fns -> [Parameter field Value NumberError Fns] -> Expression field Value NumberError Fns
op name es = ExprApp name es

valueDouble :: Double -> Expression field Value NumberError Fns
valueDouble = ExprLit . ValDouble

fromValueDouble :: Value -> Double
fromValueDouble (ValDouble x)   = x
-- TODO: Error in the Eval monad
fromValueDouble _               = error "Value is not a Double"

valueInt :: Int -> Expression field Value NumberError Fns
valueInt = ExprLit . ValInt

fromValueInt :: Value -> Int
fromValueInt (ValInt x) = x
fromValueInt _          = error "Value is not an Int"

valueBool :: Bool -> Expression field Value NumberError Fns
valueBool = ExprLit . ValBool

fromValueBool :: Value -> Bool
fromValueBool (ValBool x) = x
fromValueBool _          = error "Value is not a Boolean"

intOpHandler :: (Int -> Int -> Eval field Value NumberError Fns Int) -> [Value] -> Eval field Value NumberError Fns Value
intOpHandler op [ValInt x, ValInt y] = fmap ValInt (op x y)

doubleOpHandler :: (Double -> Double -> Eval field Value NumberError Fns Double) -> [Value] -> Eval field Value NumberError Fns Value
doubleOpHandler op [ValDouble x, ValDouble y] = fmap ValDouble (op x y)


intNameOp :: [(Fns, Int -> Int -> Eval field Value NumberError Fns Int)]
intNameOp  = [
    (Plus, \x y -> return (x + y)),
    (Minus, \x y -> return (x - y)),
    (Times, \x y -> return (x * y)),
    (Divide, intDivision)]

doubleNameOp :: [(Fns, Double -> Double -> Eval field Value NumberError Fns Double)]
doubleNameOp  = [
    (Plus, \x y -> return (x + y)),
    (Minus, \x y -> return (x - y)),
    (Times, \x y -> return (x * y)),
    (Divide, doubleDivision)]



intDivision :: Int -> Int -> Eval field Value NumberError Fns Int
intDivision x y |  y == 0 = tError $ DivideByZeroError "div 0" -- throw error
                |  otherwise = return $ x `div` y

doubleDivision :: Double -> Double -> Eval field Value NumberError Fns Double
doubleDivision x y  |  y == 0 = tError $ DivideByZeroError "div 0"-- throw error
                    |  otherwise = return $ x / y

compareOperator :: Ord a => (a -> a -> Bool) -> a -> a -> Value
compareOperator op x y  = (ValBool (x `op` y))


instance (HackcelError NumberError field, Ord field, FieldRange field) => Apply field Value NumberError Fns where
        apply Sum p   =  do
                        (f1, f2) <- expectRange (head p)
                        x <- getValue f1
                        case x of
                            ValInt _    -> sumInt f1 f2
                            ValDouble _ -> sumDouble f1 f2
                            _           -> tError ErrorInvalidType
        apply  EQ  args   = do
                            x <- expectValue (head args)
                            y <- expectValue (args !! 1)
                            return (ValBool $ x == y)
        apply  Not args = do
                            x <- expectValue (head args)
                            case x of
                                ValBool x -> return $ ValBool (not x)
                                _ -> tError ErrorInvalidType
        apply  LT args = do
                            x <- expectValue (head args)
                            y <- expectValue (args !! 1)
                            let xy = (x, y)
                            case xy of
                                (ValInt x', ValInt y') -> return (ValBool $ x' < y')
                                (ValDouble x', ValDouble y') -> return (ValBool $ x' < y')
                                _ -> tError ErrorInvalidType
        apply  GT args =   do
                            x <- expectValue (head args)
                            y <- expectValue (args !! 1)
                            let xy = (x, y)
                            case xy of
                                (ValInt x', ValInt y') -> return (ValBool $ x' > y')
                                (ValDouble x', ValDouble y') -> return (ValBool $ x' > y')
                                _ -> tError ErrorInvalidType
        apply  GE  args =  do
                                ValBool gt <- apply GT args
                                ValBool eq <- apply EQ args
                                return (ValBool $ gt || eq)
        apply  LE  args =  do
                                ValBool lt <- apply LT args
                                ValBool eq <- apply EQ args
                                return (ValBool $ lt || eq)
        apply  If  p   = do
                            cond <- expectValue (head p)
                            case cond of
                                ValBool _ -> if cond == ValBool True then
                                                expectValue (p !! 1)
                                            else
                                                expectValue (p !! 2)
                                _ -> tError ErrorInvalidType
        apply  name  p   =  do
                            x <- expectValue (head p)
                            y <- expectValue (p !! 1)
                            case x of
                                ValInt _    -> intOpHandler (op intNameOp) [x, y]
                                ValDouble _ -> doubleOpHandler (op doubleNameOp) [x, y]
                                ValBool _   -> booleanHandler name [x, y]
                            where op lst = fromJust $ lookup name lst

rangeHandler :: (FieldRange field, Ord field, HackcelError NumberError field)
            => ([Value]
            -> Value)
            -> field
            -> field
            -> Eval field Value NumberError Fns Value
rangeHandler f f1 f2 = do
                        values <- mapM getValue (getRange f1 f2)
                        if not $ isUniformType values then
                            tError $ ErrorInvalidType
                        else
                            return $ f values


sumInt :: (FieldRange field, Ord field, HackcelError NumberError field)
        => field
        -> field
        -> Eval field Value NumberError Fns Value
sumInt f1 f2 = rangeHandler s f1 f2
                where
                    s values = ValInt $ foldr (+) 0 $ map fromInt values
                    fromInt (ValInt x) = x

sumDouble :: (FieldRange field, Ord field, HackcelError NumberError field) => field -> field -> Eval field Value NumberError Fns Value
sumDouble f1 f2 = rangeHandler s f1 f2
                where
                    s values = ValDouble $ foldr (+) 0 $ map fromInt values
                    fromInt (ValDouble x) = x

booleanHandler ::   Fns
                ->  [Value]
                ->  Eval field Value NumberError Fns Value
booleanHandler And [(ValBool b1), (ValBool b2)]   = return (ValBool $ b1 && b2)
booleanHandler Or [(ValBool b1), (ValBool b2)]    = return (ValBool $ b1 || b2)
