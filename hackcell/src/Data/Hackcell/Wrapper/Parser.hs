{-# language FlexibleContexts, RankNTypes, TupleSections #-}
-- | Parses Expressions and Files for `NumberTable`
module Data.Hackcell.Wrapper.Parser
    (   parseFile
    ,   parseExpression
    ,   parseFE
    ,   parseField
    )
where

import Prelude hiding (LT, GT, EQ)

import Data.List
import Data.Maybe

import Data.Hackcell.Core
import Data.Hackcell.Wrapper.Numbers
import Data.Hackcell.Wrapper.NumberTable

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
pBool = True <$ pToken "True" <<|> False <$ pToken "False"

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
    f column row = Field (foldl (\x y -> 1 + x * 26 + y) 0 column, row)

-- Parses an expression of a left associative infix binary operator
pOperatorLeft :: Parser Fns -> Parser Expression' -> Parser Expression'
pOperatorLeft token p = (\left right -> right left) <$> p <*> pRight
  where
    pRight :: Parser (Expression' -> Expression')
    pRight = pSpaces *> ((\fn arg rest left -> rest $ ExprApp fn [PExpr left, PExpr arg]) <$ pSpaces <*> token <* pSpaces <*> p <*> pRight
        <<|> pReturn id)

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
      [Plus, Minus, Times, Divide, If, LT, LE, GT, GE, NE, Not, EQ, And, Or]

pFieldRange :: Parser (Field, Field)
pFieldRange = (,) <$> pField <* pToken ";" <*> pField

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
        <|> NE <$ pToken "!="

pAnd :: Parser Expression'
pAnd = pOperatorLeft (And <$ pToken "&&") pCompare

pOr :: Parser Expression'
pOr = pOperatorLeft (Or <$ pToken "||") pAnd

pIfThenElse :: Parser Expression'
pIfThenElse = f <$> pOr <* pSpaces <*> ((\e1 e2 -> Just (e1, e2)) <$ pToken "?" <*> pExpression <* pToken ":" <*> pExpression <<|> pReturn Nothing)
  where
    f l Nothing = l
    f l (Just (e1, e2)) = ExprApp If [PExpr l, PExpr e1, PExpr e2]

pExpression :: Parser Expression'
pExpression = pSpaces *> pIfThenElse <* pSpaces

-- | Parses a single expression. and returns an expression and a Bool, which is True if the parsing was sucessfull,
-- False if the error was automatically corrected
parseExpression :: String -> (Expression', Bool)
parseExpression str = (expr, null errors)
  where
    (expr, errors) = parse ((,) <$> pExpression <*> pEnd) $ createStr (LineCol 0 0) str

pLine :: Parser (Field, Expression')
pLine = (,) <$> pField <* pToken ": =" <*> pExpression <* pToken ":" <* pMunch (/='\n')

pFile :: Parser [(Field, Expression')]
pFile = pListSep (pToken "\n") pLine

-- | Parses a single file to a list of (Field, Expression') and a Bool, which is True if the parsing was succesfull and False
-- if error correction was applied
parseFile :: String -> ([(Field, Expression')], Bool)
parseFile str = (file, null errors)
  where
    (file, errors) = parse ((,) <$> pFile <*> pEnd) $ createStr (LineCol 0 0) str

pFE :: Parser (Field, Expression')
pFE = flip (,) <$> pExpression <* pSpaces <* pToken "@@" <* pSpaces <*> pField

-- | Parser an expression @@  field
parseFE :: String -> Maybe (Field, Expression')
parseFE str | null errors = Just fe
            | otherwise = Nothing
  where
    (fe, errors) = parse ((,) <$> pFE <*> pEnd) $ createStr (LineCol 0 0) str

-- | Parser a field
parseField :: String -> Maybe Field
parseField str | null errors = Just fe
               | otherwise = Nothing
  where
    (fe, errors) = parse ((,) <$> pField <*> pEnd) $ createStr (LineCol 0 0) str
