{-# language FlexibleContexts, RankNTypes, TupleSections #-}

module Data.Hackcel.Wrapper.Parser where

import Prelude hiding (LT, GT, EQ)

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

pBool :: Parser Bool
pBool = True <$ pToken "true" <<|> False <$ pToken "false"

pDecimals :: Parser Double
pDecimals = (\digits -> read digits / (10 ^^ length digits)) <$> pListNonEmpty pDigit

pValue :: Parser Value
pValue = (\l r -> r l) <$> pInt <*> (pWith <<|> pWithout) <<|> ValBool <$> pBool
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
    f column row = FieldInt (foldl (\x y -> 1 + x * 26 + y) 0 column, row)

-- Parses an expression of a left associative infix binary operator
pOperatorLeft :: Parser Fns -> Parser Expression' -> Parser Expression'
pOperatorLeft token p = (\left right -> right left) <$> p <* pSpaces <*> pRight
  where
    pRight :: Parser (Expression' -> Expression')
    pRight = (\fn arg rest left -> rest $ ExprApp fn [PExpr left, PExpr arg]) <$ pSpaces <*> token <* pSpaces <*> p <*> pRight
        <<|> pReturn id

pOperator :: Parser Fns -> Parser Expression' -> Parser Expression'
pOperator token p = f <$> p <*> ((\fn e -> Just (fn, e)) <$ pSpaces <*> token <* pSpaces <*> p <<|> pReturn Nothing)
  where
    f e1 Nothing = e1
    f e1 (Just (fn, e2)) = ExprApp fn [PExpr e1, PExpr e2]

pSimple :: Parser Expression'
pSimple = ExprLit <$> pValue
      <|> ExprField <$> pField
      <|> pApl
      <|> pSum
      <|> pToken "(" *> pExpression <* pToken ")"

pNot :: Parser Expression'
pNot = (\e -> ExprApp Not [PExpr e]) <$ pToken "!" <* pSpaces <*> pNot <<|> pSimple

pApl :: Parser Expression'
pApl = ExprApp <$> operators <*
  pToken "(" <*> pListSep (pToken ",") (PExpr <$> pExpression) <* pToken ")"
  where
    operators :: Parser Fns
    operators = foldr (\ap b -> ap <$ pToken (show ap) <|> b) pFail
      [Plus, Minus, Times, Divide]

pFieldRange :: Parser (Field, Field)
pFieldRange = (,) <$> pField <*> pField

pSum :: Parser Expression'
pSum = (\(f1, f2) -> ExprApp Sum [PRange f1 f2]) <$ pToken "Sum" <* pSpaces <* pToken "(" <*> pFieldRange <* pToken ")"

pPlusMinus :: Parser Expression'
pPlusMinus = pOperatorLeft token pNot
  where
    token :: Parser Fns
    token = Plus <$ pToken "+" <|> Minus <$ pToken "-"

pMultiply :: Parser Expression'
pMultiply = pOperatorLeft token pPlusMinus
  where
    token :: Parser Fns
    token = Times <$ pToken "*" <|> Divide <$ pToken "/"

pCompare :: Parser Expression'
pCompare = pOperator token pMultiply
  where
    token :: Parser Fns
    token = LT <$ pToken "<"
        <|> LE <$ pToken "<="
        <|> GT <$ pToken ">"
        <|> GE <$ pToken ">="
        <|> EQ <$ pToken "=="

pAnd :: Parser Expression'
pAnd = pOperatorLeft (And <$ pToken "&&") pCompare

pOr :: Parser Expression'
pOr = pOperatorLeft (Or <$ pToken "||") pAnd

pIfThenElse :: Parser Expression'
pIfThenElse = f <$> pOr <*> ((\e1 e2 -> Just (e1, e2)) <$ pSpaces <* pToken "?" <*> pExpression <* pToken ":" <*> pExpression <<|> pReturn Nothing)
  where
    f l Nothing = l
    f l (Just (e1, e2)) = ExprApp If [PExpr l, PExpr e1, PExpr e2]

pExpression :: Parser Expression'
pExpression = pSpaces *> pIfThenElse <* pSpaces

parseExpression :: String -> (Expression', Bool)
parseExpression str = (expr, null errors)
  where
    (expr, errors) = parse ((,) <$> pExpression <*> pEnd) $ createStr (LineCol 0 0) str

pLine :: Parser (Field, Expression')
pLine = (,) <$> pField <* pToken ": =" <*> pExpression <* pToken ":" <* pMunch (/='\n')

pFile :: Parser [(Field, Expression')]
pFile = pListSep (pToken "\n") pLine

parseFile :: String -> ([(Field, Expression')], Bool)
parseFile str = (file, null errors)
  where
    (file, errors) = parse ((,) <$> pFile <*> pEnd) $ createStr (LineCol 0 0) str
