{-# Language TypeSynonymInstances #-}
{-# Language MultiParamTypeClasses #-}
module Data.Hackcel.Wrapper.NumberTable where

import Data.Hackcel.Core
import Data.Hackcel.Wrapper.Numbers
import Data.Hackcel.Wrapper.DSL

import Data.Either
import qualified Data.Map.Strict as M
import Data.Maybe

import System.Console.ANSI
import Control.Monad.State.Lazy
import Text.PrettyPrint.Boxes


data Field = FieldInt (Int, Int)
            deriving (Eq, Ord)

field :: (Int, Int) -> Field
field = FieldInt

fieldExpr :: (Int, Int) -> Expression Field Value NumberError Fns
fieldExpr = ExprField . field

instance Show Field where
    show (FieldInt n) = show n

instance HackcelError NumberError Field where
    errorUnknownField field = UnknownFieldError $ "Unknown field error at index " ++ show field
    errorRecursion fields = RecursionError $ "Circular referencing via " ++ concatMap show fields
    errorExpectedValueGotRange = ErrorUnexpectedValue
    errorExpectedRangeGotValue = ErrorUnexpectedRange

listToSpreadSheet :: [[Expression Field Value NumberError Fns]] -> HackcelState Field Value NumberError Fns
listToSpreadSheet xss   | not (sameLengths xss) = error "multidimensionale array not all the same size"
                        | otherwise             = createHackcel (Spreadsheet $ M.fromList $ concat $ fields xss 0 0)
                        where
                            sameLengths :: [[a]] -> Bool
                            sameLengths []  = True
                            sameLengths xss = all (\xs -> length (head xss) == length xs) xss
                            fieldCel    x c r           = x @@ field (r, c)
                            fieldRow    [] c r          = []
                            fieldRow    (x:xs) c r      = fieldCel x c r : fieldRow xs (c+1) r
                            fields      []  c r         = []
                            fields      (xs:xss) c r    = fieldRow xs 0 r : fields xss 0 (r+1)



expressions = [
              [valueInt 3, valueInt 5, valueInt 7],
              [op Plus [fieldExpr (0,2), valueInt 3], valueDouble 3.5, valueInt 6]
              ]


testSpreadsheet :: HackcelState Field Value NumberError Fns
testSpreadsheet = listToSpreadSheet expressions

data InterActiveState = InterActiveState {
                          hstate :: Maybe (HackcelState Field Value NumberError Fns)
                        , current :: Field
                        , topLeftPrint :: Field
                        , view :: View
                        , file :: Maybe FilePath}

data View = Expr | Result

printNumberTable :: InterActiveState -> String
printNumberTable ias = maybe "" (render.printer) curstate
  where
    curstate = hstate ias
    curField = current ias
    topLeftField = topLeftPrint ias
    curview = view ias
    FieldInt (l,t) = topLeftField

    printer :: HackcelState Field Value NumberError Fns -> Box
    printer state = (rowname <> (columnname // vcat left (
        map (\j -> foldr (\i b -> helper state i j <> b) nullBox [0..9]) [0..9]))
        ) // char ' ' // text ( curExpr state) // text (currResult state)

    curExpr state = case M.lookup curField (fields state) of
                       Just (e,v) -> show curField ++ " = " ++ show e
                       Nothing    -> show curField ++ " = "
    currResult state = case M.lookup curField (fields state) of
                       Just (e, Just v) -> replicate (length (show curField)) ' '
                         ++ " = " ++ show v
                       Nothing    -> " = "

    helper :: HackcelState Field Value NumberError Fns -> Int -> Int -> Box
    helper state i j = let f = FieldInt (i + l,j + t) in
      case M.lookup f (fields state) of
        Just (e, Just v) -> case curview of
                              Expr -> boxprinter f (Just e)
                              Result -> boxprinter f (Just v)
        _                -> boxprinter f (Nothing :: Maybe Field)

    columnname = foldr (\i b -> char '|' <> (text.printname) (i + l) <> b) nullBox [0..9]
    rowname = char ' ' // foldl (\b i -> b // text (replicate 8 '-') // (text.printname) (i + t))
      nullBox [0..9]

    printname n = let x = show n in take 8 $ replicate (8 - length x) ' ' ++ x

    boxprinter :: Show a => Field -> Maybe a -> Box
    boxprinter f@(FieldInt (a,b)) e = borderh // (char hline <> text content)
        where
          FieldInt (c,d) = curField

          content = maybe emptys printname e
          emptys = replicate 8 ' '
          hline = if f == curField || a == c + 1 && b == d then  '*' else '|'
          vline = if f == curField || a == c && b == d + 1 then  '*' else '-'
          borderh = text $ '+' : replicate 8 vline

interface :: IO ()
interface = do setTitle "HackCell: the Spreadsheet program in Haskell"
               clearScreen
               void $ runStateT (do help; startProgram) startState
  where
    startState = InterActiveState Nothing (FieldInt (0,0)) (FieldInt (0,0)) Result Nothing
    startProgram :: StateT InterActiveState IO ()
    startProgram = do
      arg <- liftIO getLine
      s <- get
      case arg of
       'h':_ -> do refresh; help; startProgram
       'q':_ -> return ()
       'l':fn -> case fn of
                   "" -> do refresh; putStrLnS "The load argument needs a [file]\
                            \ argument"; startProgram
                   ' ':xs -> loadfile xs
                   xs  -> do refresh; putStrLnS ("Unkown argument '" ++ 'l':xs
                               ++ "'. (Press h for the commands) \n"); startProgram
       xs     -> do refresh; putStrLnS ("Unkown argument '" ++ xs ++ "'. (Press\
                    \ h for the commands) \n"); startProgram

    mainProgram :: Bool -> StateT InterActiveState IO ()
    mainProgram refr = do
      arg <- liftIO getChar
      s <- get
      case arg of
         'h' -> do refresh; helpMain; mainProgram False
         'q' -> return ()
         'l' -> do fn <- liftIO getLine
                   case fn of
                     "" -> do refresh; putStrLnS "The load argument needs a [file]\
                              \ argument"; mainProgram False
                     ' ':xs -> loadfile xs
                     xs  -> do refresh; putStrLnS ("Unkown argument '" ++ 'l':xs
                                 ++ "'. (Press h for the commands) \n"); mainProgram False
         'S' -> do let (fileloc, hst) = (fromJust (file s), fromJust (hstate s))
                   liftIO $ writeFile fileloc (prettyprint hst)
                   putStrLnS $ "File saved under: " ++ fileloc
                   mainProgram False

         'w' -> do moveField (0,-1); mainProgram True
         's' -> do moveField (0,1); mainProgram True
         'a' -> do moveField (-1,0); mainProgram True
         'd' -> do moveField (1,0); mainProgram True
         'z' -> do s <- get
                   case view s of
                    Expr   -> put (s {view = Result})
                    Result -> put (s {view = Expr})
                   mainProgram True
         'i'  -> do xs <- liftIO getLine
                    case pExpr xs of
                      Just e  -> do let news = set (fromJust (hstate s)) (current s) e
                                    let (_, runnews) = runField (current s) news
                                    put (s {hstate = Just runnews})
                                    refresh
                                    mainProgram True
                      Nothing -> do refresh
                                    putStrLnS $ "Cannot parse " ++ xs ++
                                      " into an expression"
                                    mainProgram False

         '\n' -> do when refr refresh; mainProgram False
         xs   -> do refresh; putStrLnS ("Unkown argument '" ++ xs : "'. (Press\
                    \ h for the commands) \n"); mainProgram False

    loadfile :: String -> StateT InterActiveState IO ()
    loadfile _ = do s <- get
                    let newSH = evalAll testSpreadsheet
                    let fileloc = "test.hcl"
                    put (s {hstate = Just newSH, file = Just fileloc })
                    liftIO $ setTitle ("HackCell: the Spreadsheet program in Haskell: "
                       ++ fileloc)
                    refresh
                    putStrLnS $ "Opened file: " ++ fileloc
                    mainProgram False

    refresh :: StateT InterActiveState IO ()
    refresh = do liftIO clearScreen
                 s <- get
                 case hstate s of
                   Nothing -> return ()
                   Just hstate -> putStrLnS $ printNumberTable s

    pExpr :: String -> Maybe (Expression Field Value NumberError Fns)
    pExpr s = Just $ op Plus [fieldExpr (2,3), fieldExpr (1,5)]

    help :: StateT InterActiveState IO ()
    help = putStrLnS
          "Welcome to HackCell 2D, the console spreadsheet program for two dimensional spreadsheets. \n\
          \Helper for the HackCell 2D program.\n\
          \Commands: 'h' for this helper\n\
          \          'l file' to load a file \n\
          \          'q' to quit"

    helpMain :: StateT InterActiveState IO ()
    helpMain = putStrLnS
          "Welcome to HackCell 2D, the console spreadsheet program for two dimensional spreadsheets. \n\
          \Helper for the HackCell 2D program.\n\
          \Commands: 'h' for this helper\n\
          \          'l [file]' to load a file\
          \          'S' to save the current spreadsheet \n \n\
          \          'w' to move up\n\
          \          's' to move down\n\
          \          'a' to move left\n\
          \          'd' to move right\n\
          \          'z' to switch between viewing results and expressions\n\
          \          'i [expression]' to insert an expression\n\n\
          \          'q' to quit"

    putStrLnS :: String -> StateT InterActiveState IO ()
    putStrLnS = liftIO.putStrLn

    moveField :: (Int, Int) -> StateT InterActiveState IO ()
    moveField (x, y) = do  s <- get
                           let FieldInt (xc, yc) = current s
                           let FieldInt (xv, yv) = topLeftPrint s
                           let (newx, newy) = (xc + x, yc + y)
                           let newxv
                                | newx < xv = newx
                                | newx > xv + 9 = newx - 9
                                | otherwise     = xv
                           -- = if newx < xv then newx else
                           --      (if newx > xv + 9 then newx - 9 else xv)
                           let newyv
                                | newy < yv = newy
                                | newy > yv + 9 = newy - 9
                                | otherwise     = yv
                           let news = s {current = FieldInt (newx, newy)
                                        , topLeftPrint = FieldInt (newxv, newyv)}
                           put news
