{-# language FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Data.Hackcel.Wrapper.Numbers where

import Data.Hackcel.Core

import Control.Monad.Except
import Data.Maybe


data Value  = ValInt Int
            | ValDouble Double
            deriving Show

data NumberError    = RecursionError String
                    | UnknownFieldError String
                    | DivideByZeroError String
                    | ErrorUnexpectedValue
                    | ErrorUnexpectedRange
                    deriving (Show, Eq)

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

op :: String -> [Expression field Value NumberError] -> Expression field Value NumberError 
op name es = ExprApp name $ map PExpr es

intOpHandler :: (Int -> Int -> Eval field Value NumberError Int) -> [Value] -> Eval field Value NumberError Value
intOpHandler op [ValInt x, ValInt y] = (op x y) >>= return . ValInt

doubleOpHandler :: (Double -> Double -> Eval field Value NumberError Double) -> [Value] -> Eval field Value NumberError Value
doubleOpHandler op [ValDouble x, ValDouble y] = (op x y) >>=  return . ValDouble


intNameOp :: [(String, (Int -> Int -> Eval field Value NumberError Int))]
intNameOp  = [
    ("plus", (\x y -> return (x + y))),
    ("minus", (\x y -> return (x - y))),
    ("times", (\x y -> return (x * y))),
    ("divide", intDivision)]

doubleNameOp :: [(String, (Double -> Double -> Eval field Value NumberError Double))]
doubleNameOp  = [
    ("plus", (\x y -> return (x + y))),
    ("minus", (\x y -> return (x - y))),
    ("times", (\x y -> return (x * y))),
    ("divide", doubleDivision)]



intDivision :: Int -> Int -> Eval field Value NumberError Int
intDivision x y |  y == 0 = tError $ DivideByZeroError "div 0" -- throw error
                |  otherwise = return $ x `div` y



doubleDivision :: Double -> Double -> Eval field Value NumberError Double
doubleDivision x y  |  y == 0 = tError $ DivideByZeroError "div 0"-- throw error
                    |  otherwise = return $ x / y

numberHandler :: (HackcelError NumberError field) => String -> [Argument field Value NumberError] -> Eval field Value NumberError Value
numberHandler name p = do
                    x <- expectValue (p !! 0)
                    y <- expectValue (p !! 1)
                    case x of
                        ValInt _    -> intOpHandler (op intNameOp) [x, y]
                        ValDouble _ -> doubleOpHandler (op doubleNameOp) [x, y]
                    where op lst = fromJust $ lookup name lst



