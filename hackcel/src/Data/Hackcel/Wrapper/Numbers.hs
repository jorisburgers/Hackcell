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
                    deriving (Show)

type Eval' field = Eval field Value NumberError Value


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

numberHandler :: String -> [Argument field Value NumberError] -> Eval field Value NumberError Value
numberHandler name p = do
                    let AValue ex = p !! 0
                    let AValue ey = p !! 1
                    x <- ex
                    y <- ey
                    case x of
                        ValInt _    -> intOpHandler (op intNameOp) [x, y]
                        ValDouble _ -> doubleOpHandler (op doubleNameOp) [x, y]
                    where op lst = fromJust $ lookup name lst 

