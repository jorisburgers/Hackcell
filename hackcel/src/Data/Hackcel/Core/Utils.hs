{-# language TupleSections #-}
module Data.Hackcel.Core.Utils where

import Data.Hackcel.Core.Spreadsheet
import Data.Hackcel.Core.Eval
import Data.Hackcel.Core.Expression
import Control.Monad.Except

import qualified Data.Map as M

-- | Initializes a spreadsheet.
createHackcel :: Spreadsheet field value error -- ^ The spreadsheet
              -> App field value error         -- ^ The handler function that handles function applications
              -> HackcelState field value error
createHackcel (Spreadsheet sheet) app = HackcelState (fmap (, Nothing) sheet) app

-- | Reports an error in the eval monad.
--   The error will be shown in the current field.
tError :: error -> Eval field value error return
tError =  Eval . throwError

-- | Returns the value of an argument if it is a value.
--   It reports an error if a range was passed instead of a value.
expectValue :: HackcelError error field => Argument field value error -> Eval field value error value
expectValue (AValue e)   = e
expectValue (ARange _ _) = tError errorExpectedValueGotRange

-- | Returns the range of an argument if it is a range.
--   It reports an error if a value was passed instead of a range.
expectRange :: HackcelError error field => Argument field value error -> Eval field value error (field, field)
expectRange (AValue _)   = tError errorExpectedRangeGotValue
expectRange (ARange a b) = return (a, b)

prettyprint :: (Show field, Show value, Show error) => HackcelState field value error -> String
prettyprint hackcel = foldr printField "" $ M.toAscList allFields
  where
    allFields = fields hackcel
    printField (k, a) s = show k ++ ": " ++ printValue a ++ "\n" ++ s
    printValue (expr, Nothing)  = "=" ++ show expr ++ ": <not calculated>"
    printValue (expr, Just val) = "=" ++ show expr ++ ": " ++ case fieldValue val of
      Left e -> show e
      Right v -> show v

prettyprinter :: (Show field, Show value, Show error) => HackcelState field value error -> IO ()
prettyprinter = putStrLn . prettyprint

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
        "e a" -> do let news = calcAll s
                    prettyprinter news
                    mainProgram news
        'e':' ':rest -> case pField rest of
                          Just x -> do let (_, news) = runField x s
                                       prettyprinter news
                                       mainProgram news
                          Nothing -> do putStrLn $ "Cannot parse " ++ rest ++ " into a field"
                                        mainProgram s
        'i':' ':rest -> case pExpr rest of
                         Just (f, e) -> do let news = insertExpression s f e
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
          \          'i [expression]@@[field]' to insert an expression at a field"
