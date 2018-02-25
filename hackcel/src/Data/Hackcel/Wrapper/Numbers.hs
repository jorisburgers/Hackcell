module Data.Hackcel.Wrapper.Numbers where

import Data.Hackcel.Core

import Data.Maybe

data Value  = ValInt Int
            | ValDouble Double
            deriving Show

data NumberError    = RecursionError String
                    | UnknownFieldError String
                    | DivideByZeroError String
                    deriving Show

type Eval' field = Eval field Value NumberError Value

intOpHandler :: (Int -> Int -> Int) -> [Value] -> Eval' field
intOpHandler op [ValInt x, ValInt y] = return $ ValInt $ op x y

doubleOpHandler :: (Double -> Double -> Double) -> [Value] -> Eval' field
doubleOpHandler op [ValDouble x, ValDouble y] = return $ ValDouble $ op x y


intNameOp  = [
    ("plus", (+)),
    ("minus", (-)),
    ("times", (*)),
    ("divide", intDivision)]

doubleNameOp  = [
    ("plus", (+)),
    ("minus", (-)),
    ("times", (*)),
    ("divide", doubleDivision)]


intDivision x y |  y == 0 = 0 -- throw error
                |  otherwise = x `div` y

doubleDivision x y  |  y == 0 = 0 -- throw error
                    |  otherwise = x / y

numberHandler :: String -> [Eval' field] -> Eval' field
numberHandler name p@[ex, ey] = do
                                    x <- ex
                                    y <- ey
                                    case x of
                                        ValInt _    -> intOpHandler (op intNameOp) [x, y]
                                        ValDouble _ -> doubleOpHandler (op doubleNameOp) [x, y]
                                    where op lst = fromJust $ lookup name lst 

