{-# language FlexibleContexts, RankNTypes, TupleSections #-}

module Data.Hackcel.Wrapper.Parser where

import Data.List
import Data.Maybe

import Data.Hackcel.Core
import Data.Hackcel.Wrapper.Numbers
import Data.Hackcel.Wrapper.NumberTable

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Core
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Derived

pDigit :: Parser Char
pDigit = pRange ('0', '9')

-- Greedy parser that parses a non empty list.
pListNonEmpty :: Parser a -> Parser [a]
pListNonEmpty p = (:) <$> p <*> pList p

pInt :: Parser Int
pInt = read <$> pListNonEmpty pDigit

pDecimals :: Parser Double
pDecimals = (\digits -> read digits / (10 ^^ length digits)) <$> pListNonEmpty pDigit

pNumber :: Parser Value
pNumber = (\l r -> r l) <$> pInt <*> (pWith <<|> pWithout)
  where
    pWithout = pReturn ValInt
    pWith = (\dec left -> ValDouble (fromIntegral left + dec)) <$ pSym '.' <*> pDecimals

pSpaces :: Parser ()
pSpaces = () <$ pMunch isSpace

isSpace :: Char -> Bool
isSpace ' '  = True
isSpace '\t' = True
isSpace '\r' = True
isSpace '\n' = True
isSpace _    = False

pFieldColumnChar :: Parser Int
pFieldColumnChar = (\c -> fromJust $ elemIndex c ['A' .. ]) <$> pRange ('A', 'Z')

pField :: Parser Field
pField = f <$> pSome pFieldColumnChar <*> pInt
  where
    f column row = FieldInt (foldl (\x y -> x * 26 + y) 0 column, row)

-- Parses an expression of a left associative infix binary operator
pOperatorLeft :: Parser Fns -> Parser Expression' -> Parser Expression'
pOperatorLeft token p = (\left right -> right left) <$> p <* pSpaces <*> pRight
  where
    pRight :: Parser (Expression' -> Expression')
    pRight = (\fn arg rest left -> rest $ ExprApp fn [PExpr left, PExpr arg]) <$ pSpaces <*> token <* pSpaces <*> p <*> pRight
        <<|> pReturn id

pSimple :: Parser Expression'
pSimple = ExprLit <$> pNumber
      <|> ExprField <$> pField
      <|> pToken "(" *> pExpression <* pToken ")"

pPlusMinus :: Parser Expression'
pPlusMinus = pOperatorLeft token pSimple
  where
    token :: Parser Fns
    token = Plus <$ pToken "+" <|> Minus <$ pToken "-"

pMultiply :: Parser Expression'
pMultiply = pOperatorLeft token pPlusMinus
  where
    token :: Parser Fns
    token = Times <$ pToken "*" <|> Divide <$ pToken "/"

pExpression :: Parser Expression'
pExpression = pSpaces *> pMultiply <* pSpaces

parseExpression :: String -> (Expression', Bool)
parseExpression str = (expr, null errors)
  where
    (expr, errors) = parse ((,) <$> pExpression <*> pEnd) $ createStr (LineCol 0 0) str
