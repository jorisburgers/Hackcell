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

data Fns
    = Plus
    | Minus
    | Times
    | Divide
    deriving (Eq, Show, Ord)

type Eval' field = Eval field Value NumberError Fns Value

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

op :: Fns -> [Expression field Value NumberError Fns] -> Expression field Value NumberError Fns
op name es = ExprApp name $ map PExpr es

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

instance (HackcelError NumberError field) => Apply field Value NumberError Fns where
    apply name p = do
        -- TODO: Show error if user didn't supply two arguments
        x <- expectValue (head p)
        y <- expectValue (p !! 1)
        let op lst = fromJust $ lookup name lst
        case x of
            ValInt _    -> intOpHandler (op intNameOp) [x, y]
            ValDouble _ -> doubleOpHandler (op doubleNameOp) [x, y]
