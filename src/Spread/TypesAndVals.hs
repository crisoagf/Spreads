{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Spread.TypesAndVals where
import Numeric.Natural
import Data.Time
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder
import Data.Text.Buildable
import Data.List
import Data.Ix
import Data.Monoid
import Morte.Core
import Text.Megaparsec (ParseError)
import Data.Binary

instance Ord X where
  compare x _ = absurd x

type TypeVariable = Text
type Variable = Text
type NamedReference = Text
newtype CellIndex = CellIndex { getCellIndex :: (Natural, Natural) } deriving (Eq, Show, Ord, Ix, Binary)

type CellExpr i = Expr (CellValue i)
type CellRawExpr i = Expr (CellRawValue i)
data CellError i = PE (ParseError Char X) | TE TypeError | RE (RefError i) deriving (Show)

data RefError i =
  Empty                                                              |
  NamedRefDoesNotExist NamedReference                                |
  RefDoesNotExist i                                                  |
  ErrorInRef i (CellError i)                                         |
  WrongTypeInRef (CellRawExpr i) (CellRawExpr i) |
  Loop [i] deriving (Show)

data CellRawValue i =
  TypeValue RawType             |
  IntValue Int                  |
  FloatValue Double             |
  StringValue Text              |
  DateValue Day                 |
  TimeValue UTCTime             |
  TimeDiffValue NominalDiffTime |
  PosValue i                    |
  LiftHaskFun TypeVariable (CellRawExpr i) (CellRawExpr i) (ShowWrap (CellRawExpr i -> CellExpr i)) deriving Show

instance Eq i => Eq (CellRawValue i) where
  TypeValue     x == TypeValue     y = x == y
  IntValue      x == IntValue      y = x == y
  FloatValue    x == FloatValue    y = x == y
  StringValue   x == StringValue   y = x == y
  DateValue     x == DateValue     y = x == y
  TimeValue     x == TimeValue     y = x == y
  TimeDiffValue x == TimeDiffValue y = x == y
  PosValue      x == PosValue      y = x == y
  LiftHaskFun tv t1 t2 (ShowWrap s _) == LiftHaskFun tv' t1' t2' (ShowWrap s' _) = (tv,t1,t2,s) == (tv',t1',t2',s')
  _ == _ = False

data RawType = Int | Float | String | Date | Time | TimeDiff | Pos | List deriving (Show, Eq, Ord)
type CellType = Expr RawType
newtype OrdWrap x = OrdWrap {getOrdWrap :: x} deriving (Show, Eq)

instance Ord x => Ord (OrdWrap (Expr x)) where
  compare (OrdWrap (Const c    )) (OrdWrap (Const d     )) = compare (OrdWrap c) (OrdWrap d)
  compare (OrdWrap (Const _    ))  _                       = LT
  compare (OrdWrap (Var (V x n))) (OrdWrap (Var (V y m) )) = compare (x,n) (y,m)
  compare (OrdWrap (Var  _     )) (OrdWrap (Const _     )) = GT
  compare (OrdWrap (Var  _     ))  _                       = LT
  compare (OrdWrap (Lam x y z  )) (OrdWrap (Lam x' y' z')) = compare (x, OrdWrap y, OrdWrap z) (x', OrdWrap y', OrdWrap z')
  compare (OrdWrap (Lam _ _ _  )) (OrdWrap (Const _     )) = GT
  compare (OrdWrap (Lam _ _ _  )) (OrdWrap (Var _       )) = GT
  compare (OrdWrap (Lam _ _ _  ))  _                       = LT
  compare (OrdWrap (Pi x y z   )) (OrdWrap (Pi x' y' z' )) = compare (x, OrdWrap y, OrdWrap z) (x', OrdWrap y', OrdWrap z')
  compare (OrdWrap (Pi _ _ _   )) (OrdWrap (Lam _ _ _   )) = GT
  compare (OrdWrap (Pi _ _ _   )) (OrdWrap (Const _     )) = GT
  compare (OrdWrap (Pi _ _ _   )) (OrdWrap (Var _       )) = GT
  compare (OrdWrap (Pi _ _ _   ))  _                       = LT
  compare (OrdWrap (App x y    )) (OrdWrap (App x' y'   )) = compare (OrdWrap x, OrdWrap y) (OrdWrap x', OrdWrap y')
  compare (OrdWrap (App _ _    )) (OrdWrap (Pi _ _ _    )) = GT
  compare (OrdWrap (App _ _    )) (OrdWrap (Lam _ _ _   )) = GT
  compare (OrdWrap (App _ _    )) (OrdWrap (Const _     )) = GT
  compare (OrdWrap (App _ _    )) (OrdWrap (Var _       )) = GT
  compare (OrdWrap (App _ _    ))  _                       = LT
  compare (OrdWrap (Embed x    )) (OrdWrap (Embed y     )) = compare x y
  compare (OrdWrap (Embed _    ))  _                       = GT

instance Ord (OrdWrap Const) where
  compare (OrdWrap Star) (OrdWrap Box ) = LT
  compare (OrdWrap Box ) (OrdWrap Star) = GT
  compare (OrdWrap _   ) (OrdWrap _   ) = EQ
  
data ShowWrap w = ShowWrap String w
newtype ErrorWrap a b = ErrorWrap {getErrorWrap :: Either a b}

instance Show (ShowWrap w) where
  show (ShowWrap s _) = s

instance Eq (ShowWrap w) where
  ShowWrap s1 _ == ShowWrap s2 _ = s1 == s2

instance Ord (ShowWrap w) where
  compare (ShowWrap s1 _) (ShowWrap s2 _) = compare s1 s2

instance (Buildable a, Buildable b) => Buildable (ErrorWrap a b) where
  build (ErrorWrap (Left  a)) = "Error: " <> build a
  build (ErrorWrap (Right b)) = build b

instance Show i => Buildable (CellError i) where
  build (PE e) = fromString (show e)
  build (TE e) = build e
  build (RE e) = fromString (show e)

data CellValue i =
  RawValue (CellRawValue i)           |
  RefValue i                          |
  ListValue (CellExpr i) [CellExpr i] |
  NamedRefValue NamedReference        deriving Show

instance Buildable CellIndex where
  build (CellIndex (i1, i2)) = fromString (show i1) <> "," <> fromString (show i2)

instance Buildable ind => Buildable (CellRawValue ind) where
  build (TypeValue t          ) = build t
  build (IntValue i           ) = fromString (show i)
  build (FloatValue f         ) = fromString (show f)
  build (StringValue t     ) = fromLazyText (T.cons '\"' (T.append t "\""))
  build (DateValue d     ) = fromString (show d)
  build (TimeValue t     ) = fromString (show t)
  build (TimeDiffValue t ) = build t
  build (PosValue ci) = "@{" <> build ci <> "}"
  build (LiftHaskFun _ _ _ (ShowWrap s _)) = fromString s

data CellWithType i = CellWithType { cellType :: CellType, cellValue :: CellRawExpr i } deriving Show

instance Buildable ind => Buildable (CellValue ind) where
  build (RawValue r       ) = build r
  build (RefValue ind ) = "@!{" <> build ind <> "}"
  build (NamedRefValue ref) = fromLazyText ref
  build (ListValue _ x) = "[" <> mconcat (intersperse "," (map build x)) <> "]"

-- A list will be a fold
listTypeOf :: Expr t -> Expr t
listTypeOf t = Pi "b" (Const Star) (Pi "accum" (Pi "_" t (Pi "_" (Var (V "b" 0)) (Var (V "b" 0)))) (Pi "init" (Var (V "b" 0)) (Var (V "b" 0))))

listValueOf :: Expr i -> [Expr i] -> Expr i
listValueOf t [] = Lam "b" (Const Star) (Lam "_" (Pi "_" t (Pi "_" (Var (V "b" 0)) (Var (V "b" 0)))) (Lam "init" (Var (V "b" 0)) (Var (V "init" 0))))
listValueOf t (a:as) = Lam "b" (Const Star) (Lam "f" (Pi "_" t (Pi "_" (Var (V "b" 0)) (Var (V "b" 0)))) (Lam "init" (Var (V "b" 0)) (App (App (Var (V "f" 0)) a) (App (App (App (listValueOf t as) (Var (V "b" 0))) (Var (V "f" 0))) (Var (V "init" 0))))))

instance Buildable RawType where
  build t = fromString (show t)

instance Buildable (CellWithType CellIndex) where
  build (CellWithType t c) = build c <> ":" <> build t
