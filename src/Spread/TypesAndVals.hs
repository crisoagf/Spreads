{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Spread.TypesAndVals where
import Numeric.Natural
import Data.Time
import Data.Time.Calendar
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder
import Data.Text.Buildable
import Data.List
import Data.Monoid
import Morte.Core

type TypeVariable = Text
type Variable = Text
type NamedReference = Text
type CellIndex = (Natural, Natural)

type CellExpr = Expr CellValue
type CellRawExpr = Expr CellRawValue

data CellRawValue =
  TypeValue CellType               |
  IntValue Int                     |
  FloatValue Double                |
  StringValue Text                 |
  DateValue Day                    |
  TimeValue UTCTime                |
  PosValue CellIndex               |
  ListValue CellType [CellRawExpr] |
  LiftHaskFun TypeVariable CellType CellType (ShowWrap (
    (Either CellExpr CellIndex -> Either TypeError CellWithType) -> 
    CellExpr -> CellRawExpr)) deriving Show

data RawType = Int | Float | String | Date | Time | Pos | List deriving (Show, Eq, Ord)
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
  compare (OrdWrap (Embed x    ))  _                       = GT

instance Ord (OrdWrap Const) where
  compare (OrdWrap Star) (OrdWrap Box ) = LT
  compare (OrdWrap Box ) (OrdWrap Star) = GT
  compare (OrdWrap _   ) (OrdWrap _   ) = EQ
  
data ShowWrap w = ShowWrap String w

instance Show (ShowWrap w) where
  show (ShowWrap s _) = s

data CellValue =
  RawValue CellRawValue        |
  RefValue CellIndex           |
  NamedRefValue NamedReference deriving Show

instance Buildable CellRawValue where
  build (TypeValue t          ) = build t
  build (IntValue i           ) = fromString (show i)
  build (FloatValue f         ) = fromString (show f)
  build (StringValue t     ) = fromLazyText (T.cons '\"' (T.append t "\""))
  build (DateValue d     ) = fromString (show d)
  build (TimeValue t     ) = fromString (show t)
  build (PosValue (i1,i2)) = "@{" <> fromString (show i1) <> "," <> fromString (show i2) <> "}"
  build (ListValue t x) = "[" <> mconcat (intersperse "," (map build x)) <> "]"
  build (LiftHaskFun tv t1 t2 (ShowWrap s _)) = fromString s

data CellWithType = CellWithType { cellType :: CellType, cellValue :: CellRawExpr } deriving Show

instance Buildable CellValue where
  build (RawValue r       ) = build r
  build (RefValue (i1,i2) ) = "@!{" <> fromString (show i1) <> "," <> fromString (show i2) <> "}"
  build (NamedRefValue ref) = fromLazyText ref

instance Buildable RawType where
  build t = fromString (show t)

instance Buildable CellWithType where
  build (CellWithType t c) = build c <> ":" <> build t

