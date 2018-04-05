-- | Provides the functionality for an interaction with the spreadsheet via the console
module Data.Hackcell.Core.Interactive where

import Data.Hackcell.Core.Spreadsheet
import Data.Hackcell.Core.Eval
import Data.Hackcell.Core.Expression
import Data.Hackcell.Core.Utils
-- | Runs the basic interactive mode
interactive :: (Show field, Show value, Show error, Show app, HackcellError error field
               , Ord field, Apply field value error app)
            => HackcellState field value error app
            -> (String -> Maybe field)
            -> (String -> Maybe (field, Expression field value error app)) -> IO ()
interactive state pField pExpr = do help
                                    mainProgram state
  where
    -- mainProgram :: (Show field, Show value, Show error, HackcellError error field
    --                , Ord field) => HackcellState field value error -> IO ()
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
