{-# language FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Data.Hackcel.Wrapper.Numbers where

import Data.Hackcel.Core

import Control.Monad.Except
import Control.Monad.Error
import Data.Maybe

data Value  = ValInt Int
            | ValDouble Double
            deriving Show

data NumberError    = RecursionError String
                    | UnknownFieldError String
                    | DivideByZeroError String
                    deriving Show

type Eval' field = Eval field Value NumberError Value

instance MonadError NumberError (Eval field  NumberError) where
    throwError e   = error "Error occured"

intOpHandler :: (Int -> Int -> Eval field Value NumberError Int) -> [Value] -> Eval' field
intOpHandler op [ValInt x, ValInt y] = (op x y) >>= return . ValInt

doubleOpHandler :: (Double -> Double -> Eval field Value NumberError Double) -> [Value] -> Eval' field
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


intDivision x y |  y == 0 = return -1--throwError $ DivideByZeroError "div 0" -- throw error
                |  otherwise = return $ x `div` y

doubleDivision x y  |  y == 0 = return -1--throwError $ DivideByZeroError "div 0"-- throw error
                    |  otherwise = return $ x / y

numberHandler :: String -> [Eval' field] -> Eval' field
numberHandler name p@[ex, ey] = do
                                    x <- ex
                                    y <- ey
                                    case x of
                                        ValInt _    -> intOpHandler (op intNameOp) [x, y]
                                        ValDouble _ -> doubleOpHandler (op doubleNameOp) [x, y]
                                    where op lst = fromJust $ lookup name lst 

