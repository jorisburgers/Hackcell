{-# language  ScopedTypeVariables #-}
module InterfaceTable (interactiveTable, interface) where

import Data.Hackcell.Core
import Data.Hackcell.Wrapper.DSL
import Data.Hackcell.Wrapper.Numbers
import Data.Hackcell.Wrapper.NumberTable
import Data.Hackcell.Wrapper.Parser

import qualified Data.Map.Strict as M
import Data.Maybe
import System.IO
import Control.Exception

import System.Console.ANSI
import Control.Monad.State.Lazy
import Text.PrettyPrint.Boxes

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Core
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Derived

data InterActiveState = InterActiveState {
                          hstate :: Maybe (HackcellState Field Value NumberError Fns)
                        , current :: Field
                        , topLeftPrint :: Field
                        , view :: View
                        , file :: Maybe FilePath}

data View = Expr | Result

-- | An interactive debug mode for the NumberTable wrapper
interactiveTable :: HackcellState Field Value NumberError Fns -> IO ()
interactiveTable st = interactive st parseField parseFE

printNumberTable :: InterActiveState -> String
printNumberTable ias = maybe "" (render.printer) curstate
  where
    curstate = hstate ias
    curField = current ias
    topLeftField = topLeftPrint ias
    curview = view ias
    Field (l,t) = topLeftField

    printer :: HackcellState Field Value NumberError Fns -> Box
    printer state = (rowname <> (columnname // vcat left (
        map (\j -> foldr (\i b -> helper state i j <> b) nullBox [0..9]) [0..9]))
        ) // char ' ' // text ( curExpr state) // text (currResult state)

    curExpr state = case M.lookup curField (fields state) of
                       Just (e,v) -> show curField ++ " = " ++ show e
                       Nothing    -> show curField ++ " = "
    currResult state = case M.lookup curField (fields state) of
                       Just (e, Just v) -> replicate (length (show curField)) ' '
                         ++ " = " ++ show v
                       _    -> " = "

    helper :: HackcellState Field Value NumberError Fns -> Int -> Int -> Box
    helper state i j = let f = Field (i + l,j + t) in
      case M.lookup f (fields state) of
        Just (e, Just v) -> case curview of
                              Expr -> boxprinter f (Just e)
                              Result -> boxprinter f (Just v)
        _                -> boxprinter f (Nothing :: Maybe Field)

    columnname = foldr (\i b -> char '|' <> (text.printname') (columntoNumber (i + l))
      <> b) nullBox [0..9]
    rowname = char ' ' // foldl (\b i -> b // text (replicate 8 '-') // (text.printname) (i + t))
      nullBox [0..9]

    boxprinter :: Show a => Field -> Maybe a -> Box
    boxprinter f@(Field (a,b)) e = borderh // (char hline <> text content)
        where
          Field (c,d) = curField

          content = maybe emptys printname e
          emptys = replicate 8 ' '
          hline = if f == curField || a == c + 1 && b == d then  '*' else '|'
          vline = if f == curField || a == c && b == d + 1 then  '*' else '-'
          borderh = text $ '+' : replicate 8 vline

printname :: (Show a) => a -> String
printname n = let x = show n in take 8 $ replicate (8 - length x) ' ' ++ x

printname' :: String -> String
printname' n = let x = n in take 8 $ replicate (8 - length x) ' ' ++ x


-- | A command line interface for the NumberTable. Press the 'h' key (help) for
-- the options it supports.
interface :: IO ()
interface = do setTitle "HackCell: the Spreadsheet program in Haskell"
               clearScreen
               void $ runStateT (do help; startProgram) startState
  where
    startState = InterActiveState Nothing (Field (1,1)) (Field (1,1)) Result Nothing
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
                   ' ':xs -> do loadfile xs; mainProgram False
                   xs  -> do unknownArg ('l':xs); startProgram
       'n':_  -> do newFile; mainProgram False
       xs     -> do unknownArg xs; startProgram

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
                     ' ':xs -> do loadfile xs; mainProgram False
                     xs  -> do unknownArg ('l':xs); mainProgram False
         'n' -> do newFile; mainProgram False
         'S' -> do
            afn <- liftIO getLine
            case afn of
              "" -> do
                let (fileloc, hst) = (fromJust (file s), fromJust (hstate s))
                liftIO $ writeFile fileloc (prettyprint hst)
                putStrLnS $ "File saved under: " ++ fileloc
                mainProgram False
              'A':' ':fn -> do
                let hst = fromJust (hstate s)
                liftIO $ writeFile fn (prettyprint hst)
                putStrLnS $ "File saved under: " ++ fn
                liftIO $ setTitle ("HackCell: the Spreadsheet program in Haskell: "
                   ++ fn)
                put (s {file = Just fn })
                mainProgram False
              xs -> do unknownArg ('S':xs); mainProgram False
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
                    case parseExpression xs of
                      (e, True) -> do
                        insertE e
                        mainProgram True
                      (e, False) -> do
                        refresh
                        putStrLnS $ "Cannot parse " ++ xs ++ " into an expression"
                        putStrLnS $ "Did you mean \'" ++ show e ++ "\'? [y/n]"
                        answer <- liftIO getLine
                        case answer of
                          'y':_ -> do insertE e; mainProgram True
                          _     -> mainProgram True

         '\n' -> do when refr refresh; mainProgram False
         xs   -> do unknownArg [xs]; mainProgram False

unknownArg :: String -> StateT InterActiveState IO ()
unknownArg s = do
  refresh
  putStrLnS ("Unknown argument '" ++ s
    ++ "'. (Press h for the commands) \n")

insertE :: Expression' -> StateT InterActiveState IO ()
insertE e = do
  s <- get
  let news = set (fromJust (hstate s)) (current s) e
  let (_, runnews) = runField (current s) news
  let runnews' = evalAll runnews
  put (s {hstate = Just runnews'})
  refresh

newFile :: StateT InterActiveState IO ()
newFile = do
  let sprdsht = Spreadsheet (M.fromList [])
  s <- get
  let newSH = evalAll $ createHackcell sprdsht
  let fileloc = "untitled"
  put (s {hstate = Just newSH, file = Just fileloc })
  liftIO $ setTitle ("HackCell: the Spreadsheet program in Haskell: "
     ++ fileloc)
  refresh
  putStrLnS "Openend a new file"

loadfile :: String -> StateT InterActiveState IO ()
loadfile fileloc = do
  t <- liftIO (loader fileloc)
  case t of
    Just contents -> do
      let (lst, noErrors) = parseFile contents
      if noErrors then do
          let sprdsht = Spreadsheet (M.fromList lst)
          s <- get
          let newSH = evalAll $ createHackcell sprdsht
          put (s {hstate = Just newSH, file = Just fileloc })
          liftIO $ setTitle ("HackCell: the Spreadsheet program in Haskell: "
             ++ fileloc)
          refresh
          putStrLnS $ "Opened file: " ++ fileloc
        else do
          refresh
          putStrLnS "File contained errors, couldn't load it."
    Nothing -> return ()

loader :: String -> IO (Maybe String)
loader fileloc = handle
  (\(e :: IOException) -> do liftIO (print e); return Nothing)
  (do h <- openFile fileloc ReadMode
      contents <- liftIO $ hGetContents h
      return (Just contents))

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
      \          'n' for a new file\n\
      \          'q' to quit"

helpMain :: StateT InterActiveState IO ()
helpMain = putStrLnS
      "Welcome to HackCell 2D, the console spreadsheet program for two dimensional spreadsheets. \n\
      \Helper for the HackCell 2D program.\n\
      \Commands: 'h' for this helper\n\
      \          'l [file]' to load a file\n\
      \          'S' to save the current spreadsheet \n\
      \          'SA [file] to save the file to the given location \n \n\
      \          'w' to move up\n\
      \          's' to move down\n\
      \          'a' to move left\n\
      \          'd' to move right\n\
      \          'z' to switch between viewing results and expressions\n\
      \          'i [expression]' to insert an expression at the current position\n\n\
      \          'q' to quit"

putStrLnS :: String -> StateT InterActiveState IO ()
putStrLnS = liftIO.putStrLn

moveField :: (Int, Int) -> StateT InterActiveState IO ()
moveField (x, y) = do  s <- get
                       let Field (xc, yc) = current s
                       let Field (xv, yv) = topLeftPrint s
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
                       let news = s {current = Field (newx, newy)
                                    , topLeftPrint = Field (newxv, newyv)}
                       put news
