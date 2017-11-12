{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Spread.StdLib.Int where
import Spread.TypesAndVals
import Morte.Core

basicIntFns :: [(NamedReference, CellRawExpr)]
basicIntFns = [
  ("Sum", Embed (LiftHaskFun "" (App (Embed List) (Embed Int)) (Embed Int) (ShowWrap "Sum" (\ evalFn -> either
    (error "Sum: The Spread type system has failed us, it gave us something that does not type check!")
    (Embed . IntValue . sum . (\case
      CellWithType _ (Embed (ListValue (Embed Int) l)) -> fmap (\ case
        Embed (IntValue i) -> i
        x -> error ("Sum: The Spread type system has failed us! We wanted an Int, but it gave us " ++ show x)) l
      x -> error ("Sum: The Spread type system has failed us! We wanted a List, but it gave us " ++ show x))) . evalFn . Left)))),
  ("Plus", Embed (LiftHaskFun "" (Embed Int) (Pi "" (Embed Int) (Embed Int)) (ShowWrap "Plus" (\ evalFn -> either
    (error "Plus: The Spread type system has failed us, it gave us something that does not type check!")
    (\case
        CellWithType _ (Embed (IntValue i)) -> Embed $ LiftHaskFun "" (Embed Int) (Embed Int) (ShowWrap ("Plus " ++ show i) (\ evalFn -> either
          (error "Plus: The Spread type system has failed us, it gave us something that does not type check!")
          (Embed . IntValue . (\ case
            CellWithType _ (Embed (IntValue j)) -> i + j
            x -> error ("Plus: The Spread type system has failed us! We wanted an Int, but it gave us " ++ show x))) . evalFn . Left))
        x -> error ("Plus: The Spread type system has failed us! We wanted an Int, but it gave us " ++ show x)) . evalFn . Left))))
  ]
