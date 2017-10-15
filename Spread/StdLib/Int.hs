module Spread.StdLib.Int where

basicFns = [
  ("sum", LiftHaskFun (ListType IntType) IntType (ShowWrap (\ evalFn -> either
    (error "The Spread type system has failed us, it told us it gave us a list, but didn't type check it!")
    (RawValue . IntValue . sum . (\case
      ListValue l -> fmap (\ case
        IntValue i -> i
        x -> error ("The Spread type system has failed us! We wanted a List, but it gave us " ++ show x)) l
      x -> error ("The Spread type system has failed us! We wanted a List, but it gave us " ++ show x))) . evalFn (ListType IntType))))]
