{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Spread.StdLib.Int where
import Spread.TypesAndVals

basicIntFns :: [(NamedReference, CellValue errortype)]
basicIntFns = [
  ("sum", RawValue (LiftHaskFun (ListType IntType) IntType (ShowWrap (\ evalFn -> either
    (error "The Spread type system has failed us, it gave us something that does not type check!")
    (RawValue . IntValue . sum . (\case
      (ListType IntType, ListValue l) -> fmap (\ case
        IntValue i -> i
        x -> error ("The Spread type system has failed us! We wanted an Int, but it gave us " ++ show x)) l
      x -> error ("The Spread type system has failed us! We wanted a List, but it gave us " ++ show x))) . evalFn)))),
  ("plus", RawValue (LiftHaskFun IntType (FuncType IntType IntType) (ShowWrap (\ evalFn -> either
    (error "The Spread type system has failed us, it gave us something that does not type check!")
    (\case
        (IntType, IntValue i) -> RawValue $ LiftHaskFun IntType IntType (ShowWrap (\ evalFn -> either
          (error "The Spread type system has failed us, it gave us something that does not type check!")
          (RawValue . IntValue . (\ case
            (IntType, IntValue j) -> i + j
            x -> error ("The Spread type system has failed us! We wanted an Int, but it gave us " ++ show x))) . evalFn))
        x -> error ("The Spread type system has failed us! We wanted an Int, but it gave us " ++ show x)) . evalFn))))
  ]
