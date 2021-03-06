module InvalidateTests where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Hackcell.Core
import Data.Hackcell.Wrapper.DSL
import Data.Hackcell.Wrapper.NumberList
import Data.Hackcell.Wrapper.Numbers

import Data.Map.Strict

spreadsheet :: HackcellState Field Value NumberError Fns
spreadsheet = evalAll $ createHackcell $ Spreadsheet $ fromList
    -- Literals
  [ ExprLit (ValInt 0) @@ field 1
  , ExprLit (ValInt 2) @@ field 2

    -- Formulas
  , ExprApp Plus [PExpr $ ExprField $ field 1, PExpr $ ExprLit $ ValInt 1] @@ field 11
  , ExprApp Plus [PExpr $ ExprField $ field 2, PExpr $ ExprLit $ ValInt 2] @@ field 12

  , ExprApp Plus [PExpr $ ExprField $ field 11, PExpr $ ExprField $ field 12] @@ field 21

  -- Divide by zero
  , ExprApp Divide [PExpr $ ExprLit $ ValInt 1, PExpr $ ExprField $ field 1] @@ field 25
  , ExprField (field 25) @@ field 26

  -- Circular dependency
  , ExprField (field 33) @@ field 31
  , ExprField (field 31) @@ field 32
  , ExprField (field 32) @@ field 33
  ]

invalidates :: HackcellState Field Value NumberError Fns -> Field -> Field -> Bool
invalidates s updateField checkField = not $ isEvaluated s' checkField
  where
    s' = set s updateField $ ExprLit $ ValInt 1

shouldInvalidate :: HackcellState Field Value NumberError Fns -> Field -> Field -> TestTree
shouldInvalidate s updateField checkField = testProperty
  ("Setting " ++ show updateField ++ " should invalidate " ++ show checkField)
  $ invalidates s updateField checkField

shouldNotInvalidate :: HackcellState Field Value NumberError Fns -> Field -> Field -> TestTree
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
    , (field 1, field 25)
    , (field 25, field 26)

    -- Indirect
    , (field 1, field 21)
    , (field 2, field 21)
    , (field 1, field 26)

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
