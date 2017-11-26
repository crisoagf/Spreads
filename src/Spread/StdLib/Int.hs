{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Spread.StdLib.Int where
import Spread.StdLib
import Spread.TypesAndVals
import Morte.Core
import Data.Monoid
import Data.Text.Buildable

plus :: (Buildable i, Show i) => CellRawExpr i
plus = liftHaskFnInt "Plus" (Pi "" (Embed $ TypeValue Int) (Embed $ TypeValue Int)) (\ i -> fmap RawValue $
         liftHaskFnInt ("Plus " <> pretty i) (Embed $ TypeValue Int) (\ j -> Embed $ RawValue $ IntValue $ i + j))

basicIntFns :: (Buildable i, Show i) => [(NamedReference, CellRawExpr i)]
basicIntFns = [
  ("Sum", Lam "x" (listTypeOf (Embed $ TypeValue Int)) (App (App (App (Var (V "x" 0)) (Embed (TypeValue Int))) plus) (Embed (IntValue 0)))),
  ("Plus", plus)
  ]
