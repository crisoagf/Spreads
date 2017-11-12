{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Spread.StdLib where
import Spread.TypesAndVals
import Morte.Core
import Data.Text.Lazy

liftHaskFnType :: Text -> Text -> CellType -> (CellType -> CellRawExpr) -> CellRawExpr
liftHaskFnType fname tname restype f = Embed $ LiftHaskFun tname (Const Star) restype (ShowWrap (unpack fname) (\ evalFn -> either
    (error (unpack fname ++ ": The Spread type system has failed us, it gave us something that does not type check!"))
    (\case
      CellWithType (Const Star) (Embed (TypeValue t)) -> f t
      CellWithType t x -> error (unpack fname ++ ": The Spread type system has failed us! We wanted a Type, but it gave us a " ++ show t)) . evalFn . Left))

liftHaskFnPos :: Text -> CellType -> ((Either CellExpr CellIndex -> Either TypeError CellWithType) -> CellIndex -> CellRawExpr) -> CellRawExpr
liftHaskFnPos fname restype f = Embed $ LiftHaskFun "" (Embed Pos) restype (ShowWrap (unpack fname) (\ evalFn x -> either
    (error (unpack fname ++ ": The Spread type system has failed us, by giving us " ++ show x ++ " that does not type check!"))
    (\case
      CellWithType (Embed Pos) (Embed (PosValue i)) -> f evalFn i
      CellWithType t x -> error (unpack fname ++ ": The Spread type system has failed us! We wanted a Pos, but it gave us a " ++ show t)) $ evalFn $ Left x))
