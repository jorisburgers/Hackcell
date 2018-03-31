module Data.Hackcel.Core.Interactive where

import Data.Hackcel.Core.Spreadsheet
import Data.Hackcel.Core.Eval
import Data.Hackcel.Core.Expression
import Data.Hackcel.Core.Utils

interactive :: (Show field, Show value, Show error, HackcelError error field
               , Ord field) => HackcelState field value error -> (String -> Maybe field)
            -> (String -> Maybe (field, Expression field value error)) -> IO ()
interactive state pField pExpr = do help
                                    mainProgram state
  where
    -- mainProgram :: (Show field, Show value, Show error, HackcelError error field
    --                , Ord field) => HackcelState field value error -> IO ()
    mainProgram s = do
      arg <- getLine
      case arg of
        "h" -> do help; mainProgram s
        "p" -> do prettyprinter s; mainProgram s
        "q" -> return ()
        "e a" -> do let news = evalAll s
                    prettyprinter news
                    mainProgram news
        'e':' ':rest -> case pField rest of
                          Just x -> do let (_, news) = runField x s
                                       prettyprinter news
                                       mainProgram news
                          Nothing -> do putStrLn $ "Cannot parse " ++ rest ++ " into a field"
                                        mainProgram s
        's':' ':rest -> case pExpr rest of
                         Just (f, e) -> do let news = set s f e
                                           prettyprinter news
                                           mainProgram news
                         Nothing -> do putStrLn $ "Cannot parse " ++ rest ++ " into an expression"
                                       mainProgram s
        _   -> do putStrLn "Unkown argument"; mainProgram s

    help :: IO ()
    help = putStrLn
          "Welcome to the interactive HackCell shell.\n\
          \Commands: 'h' for this helper\n\
          \          'p' to print\n\
          \          'q' to quit\n\
          \          'e [field]' to evaluate a field\n\
          \          'e a' to evaluate all fields\n\
          \          's [expression]@@[field]' to insert or update an expression at a field"
