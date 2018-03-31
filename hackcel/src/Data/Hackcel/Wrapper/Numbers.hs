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
            deriving (Show, Eq)

data NumberError    = RecursionError String
                    | UnknownFieldError String
                    | DivideByZeroError String
                    | ErrorUnexpectedValue
                    | ErrorUnexpectedRange
                    | ErrorInvalidType
                    deriving (Show, Eq)

instance TypeEq Value where
    typeEq (ValInt _) (ValInt _) = True
    typeEq (ValDouble _) (ValDouble _) = True
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

op :: String -> [Parameter field Value NumberError] -> Expression field Value NumberError
op name es = ExprApp name es

intOpHandler :: (Int -> Int -> Eval field Value NumberError Int) -> [Value] -> Eval field Value NumberError Value
intOpHandler op [ValInt x, ValInt y] = fmap ValInt (op x y)

doubleOpHandler :: (Double -> Double -> Eval field Value NumberError Double) -> [Value] -> Eval field Value NumberError Value
doubleOpHandler op [ValDouble x, ValDouble y] = fmap ValDouble (op x y)


intNameOp :: [(String, Int -> Int -> Eval field Value NumberError Int)]
intNameOp  = [
    ("plus", \x y -> return (x + y)),
    ("minus", \x y -> return (x - y)),
    ("times", \x y -> return (x * y)),
    ("divide", intDivision)]

doubleNameOp :: [(String, Double -> Double -> Eval field Value NumberError Double)]
doubleNameOp  = [
    ("plus", \x y -> return (x + y)),
    ("minus", \x y -> return (x - y)),
    ("times", \x y -> return (x * y)),
    ("divide", doubleDivision)]



intDivision :: Int -> Int -> Eval field Value NumberError Int
intDivision x y |  y == 0 = tError $ DivideByZeroError "div 0" -- throw error
                |  otherwise = return $ x `div` y



doubleDivision :: Double -> Double -> Eval field Value NumberError Double
doubleDivision x y  |  y == 0 = tError $ DivideByZeroError "div 0"-- throw error
                    |  otherwise = return $ x / y

numberHandler :: (FieldRange field, Ord field, HackcelError NumberError field) => String -> [Argument field Value NumberError] -> Eval field Value NumberError Value
numberHandler name p =  
                        if name == "sum" then
                            do 
                            (f1, f2) <- expectRange (head p)
                            x <- getValue f1
                            case x of
                                ValInt _ -> sumInt f1 f2
                                ValDouble _ -> sumDouble f1 f2
                        else
                            do
                            x <- expectValue (head p)
                            y <- expectValue (p !! 1)
                            case x of
                                ValInt _    -> intOpHandler (op intNameOp) [x, y]
                                ValDouble _ -> doubleOpHandler (op doubleNameOp) [x, y]
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

