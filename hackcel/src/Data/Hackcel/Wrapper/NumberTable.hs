{-# Language TypeSynonymInstances #-}
{-# Language MultiParamTypeClasses #-}
module Data.Hackcel.Wrapper.NumberTable where

import Data.Hackcel.Core
import Data.Hackcel.Wrapper.Numbers
import Data.Hackcel.Wrapper.DSL

import Data.Either
import qualified Data.Map.Strict as M

import System.Console.ANSI
import Control.Monad.State.Lazy
import Text.PrettyPrint.Boxes

data Field = FieldInt (Int, Int)
            deriving (Eq, Ord)

field :: (Int, Int) -> Field
field = FieldInt

fieldExpr :: (Int, Int) -> Expression Field Value NumberError
fieldExpr = ExprField . field

instance Show Field where
    show (FieldInt n) = show n

instance HackcelError NumberError Field where
    errorUnknownField field = UnknownFieldError $ "Unknown field error at index " ++ show field
    errorRecursion fields   = RecursionError $ "Circular referencing via " ++ concatMap show fields
    errorExpectedValueGotRange = ErrorUnexpectedValue
    errorExpectedRangeGotValue = ErrorUnexpectedRange

listToSpreadSheet :: [[Expression Field Value NumberError]] -> HackcelState Field Value NumberError
listToSpreadSheet xss   | not (sameLengths xss) = error "multidimensionale array not all the same size"
                        | otherwise             = createHackcel (Spreadsheet $ M.fromList $ concat $ fields xss 0 0) numberHandler
                        where
                            sameLengths :: [[a]] -> Bool
                            sameLengths []  = True
                            sameLengths xss = and $ map (\xs -> length (head xss) == length xs) xss
                            fieldCel    x c r           = x @@ field (r, c)
                            fieldRow    [] c r          = []
                            fieldRow    (x:xs) c r      = fieldCel x c r : fieldRow xs (c+1) r
                            fields      []  c r         = []
                            fields      (xs:xss) c r    = fieldRow xs 0 r : fields xss 0 (r+1)



expressions = [
                [valueInt 3, valueInt 5, valueInt 7],
                [op "plus" [fieldExpr (0,2), valueInt 3], valueDouble 3.5, valueInt 6]
            ]


testSpreadshheet :: HackcelState Field Value NumberError
testSpreadshheet = listToSpreadSheet expressions

data InterActiveState = InterActiveState {
                          hstate :: Maybe (HackcelState Field Value NumberError)
                        , current :: Field
                        , topLeftPrint :: Field}

printNumberTable :: InterActiveState -> String
printNumberTable ias = maybe "" (render.printer) curstate
  where
    curstate = hstate ias
    curField = current ias
    topLeftField = topLeftPrint ias
    FieldInt (l,t) = topLeftField

    printer :: HackcelState Field Value NumberError -> Box
    printer state = (rowname <> (columnname // vcat left (
        map (\j -> foldr (\i b -> helper state i j <> b) nullBox [0..9]) [0..9]))
        ) // char ' ' // text ( curExpr state)

    curExpr state = case M.lookup curField (fields state) of
                       Just (e,v) -> show curField ++ ": $ " ++ show e
                       Nothing    -> ""

    helper :: HackcelState Field Value NumberError -> Int -> Int -> Box
    helper state i j = let f = FieldInt (i + l,j + t) in
      case M.lookup f (fields state) of
        Just (e,v) -> boxprinter f (Just v)
        Nothing    -> boxprinter f (Nothing :: Maybe Field)

    columnname = foldr (\i b -> char '|' <> (text.printname) (i + l) <> b) nullBox [0..9]
    rowname = foldl (\b i -> b // (text.printname) (i + l) // text (replicate 8 '-'))
      nullBox [0..9]

    printname n = let x = show n in replicate (8 - length x) ' ' ++ x

    boxprinter :: Show a => Field -> Maybe a -> Box
    boxprinter f@(FieldInt (a,b)) e = borderh // (char hline <> text content)
        where
          FieldInt (c,d) = curField

          content = maybe emptys (\x -> take 8 (show x ++ emptys)) e
          emptys = replicate 8 ' '
          hline = if f == curField || a == c + 1 && b == d then  '*' else '|'
          vline = if f == curField || a == c && b == d + 1 then  '*' else '-'
          borderh = text $ '+' : replicate 8 vline

interface :: IO ()
interface = do setTitle "HackCell: the Spreadsheet program in Haskell"
               clearScreen
               runStateT (do help; mainProgram) startState >> return ()
  where
    startState = InterActiveState Nothing (FieldInt (0,0)) (FieldInt (0,0))

    mainProgram :: StateT InterActiveState IO ()
    mainProgram = do
      --refresh
      arg <- liftIO getLine
      s <- get
      case arg of
         'h':_ -> do refresh; help; mainProgram
         'q':_ -> return ()
         'l':fn -> do put (s {hstate = Just testSpreadshheet}); refresh; mainProgram
         'w':_ -> do moveField (0,-1); refresh; mainProgram
         's':_ -> do moveField (0,1); refresh; mainProgram
         'a':_ -> do moveField (-1,0); refresh; mainProgram
         'd':_ -> do moveField (1,0); refresh; mainProgram
         xs    -> do refresh; putStrLnS ("Unkown argument '" ++ xs ++ "'. (Press\
                     \h for the commands) \n"); mainProgram

    refresh :: StateT InterActiveState IO ()
    refresh = do liftIO clearScreen
                 s <- get
                 case hstate s of
                   Nothing -> return ()
                   Just hstate -> putStrLnS $ printNumberTable s

    help :: StateT InterActiveState IO ()
    help = putStrLnS
          "Welcome to HackCell 2D, the console spreadsheet program for two dimensional spreadsheets. \n\
          \Helper for the HackCell 2D program.\n\
          \Commands: 'h' for this helper\n\
          \          'l file' to load a file \n\
          \          'q' to quit"

    putStrLnS :: String -> StateT InterActiveState IO ()
    putStrLnS = liftIO.putStrLn
    moveField :: (Int, Int) -> StateT InterActiveState IO ()
    moveField (x, y) = do  s <- get
                           let FieldInt (a,b) = current s
                           let news = s {current = FieldInt (a + x, b + y)}
                           put news
