module InvalidateTests where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Hackcel.Core
import Data.Hackcel.Wrapper.DSL
import Data.Hackcel.Wrapper.NumberList
import Data.Hackcel.Wrapper.Numbers

import Data.Either
import Data.Map.Strict

spreadsheet :: HackcelState Field Value NumberError Fns
spreadsheet = evalAll $ createHackcel $ Spreadsheet $ fromList
    -- Literals
  [ ExprLit (ValInt 0) @@ field 1
  , ExprLit (ValInt 2) @@ field 2
  
    -- Formulas
  , ExprApp Plus [PExpr $ ExprField $ field 1, PExpr $ ExprLit $ ValInt 1] @@ field 11
  , ExprApp Plus [PExpr $ ExprField $ field 2, PExpr $ ExprLit $ ValInt 2] @@ field 12

  , ExprApp Plus [PExpr $ ExprField $ field 11, PExpr $ ExprField $ field 12] @@ field 21

  -- Divide by zero
  , ExprApp Divide [PExpr $ ExprLit $ ValInt 1, PExpr $ ExprField $ field 1] @@ field 25

    -- Circular dependency
  {- , ExprField (field 33) @@ field 31
  , ExprField (field 31) @@ field 32
  , ExprField (field 32) @@ field 33 -}
  ]

invalidates :: HackcelState Field Value NumberError Fns -> Field -> Field -> Bool
invalidates s updateField checkField = not $ isEvaluated s' checkField
  where
    s' = set s updateField $ ExprLit $ ValInt 1

shouldInvalidate :: HackcelState Field Value NumberError Fns -> Field -> Field -> TestTree
shouldInvalidate s updateField checkField = testProperty
  ("Setting " ++ show updateField ++ " should invalidate " ++ show checkField)
  $ invalidates s updateField checkField

shouldNotInvalidate :: HackcelState Field Value NumberError Fns -> Field -> Field -> TestTree
shouldNotInvalidate s updateField checkField = testProperty
  ("Setting " ++ show updateField ++ " should not invalidate " ++ show checkField)
  $ not $ invalidates s updateField checkField

invalidateTests :: TestTree
invalidateTests = testGroup "Invalidate"
  [ testGroup "should invalidate" $ fmap (uncurry $ shouldInvalidate spreadsheet)
    -- Direct dependencies
    [ (field 1, field 11) 
    , (field 2, field 12)
    , (field 11, field 21)
    , (field 12, field 21)
    -- , (field 1, field 25)

    -- Indirect
    , (field 1, field 21)
    , (field 2, field 21)

    -- Circular dependencies
    , (field 31, field 32)
    , (field 31, field 33)
    , (field 32, field 31)
    , (field 32, field 33)
    , (field 33, field 31)
    , (field 33, field 32)
    ]
  , testGroup "should not invalidate" $ fmap (uncurry $ shouldNotInvalidate spreadsheet)
    [ (field 1, field 2)
    , (field 11, field 1)
    , (field 21, field 12)
    , (field 1, field 12)
    ]
  ]