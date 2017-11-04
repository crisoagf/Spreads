module Spread.TypesAndVals where
import Numeric.Natural
import Data.Time
import Data.Time.Calendar
import Data.Text (Text)

type TypeVariable = Text
type Variable = Text
type NamedReference = Text
type CellIndex = (Natural, Natural)

data CellType =
  VarType TypeVariable       |
  IntType                    |
  FloatType                  |
  StringType                 |
  DateType                   |
  TimeType                   |
  PosType                    |
  TypeType                   |
  ListType CellType          |
  FuncType CellType CellType deriving (Show, Eq)

data CellRawValue errortype =
  IntValue Int                          |
  FloatValue Double                     |
  StringValue Text                      |
  DateValue Day                         |
  TimeValue UTCTime                     |
  PosValue CellIndex                    |
  TypeValue CellType                    |
  ListValue [CellRawValue errortype]    |
  LiftHaskFun CellType CellType (ShowWrap (
    (Either (CellValue errortype) CellIndex -> Either errortype (CellType, CellRawValue errortype)) -> 
    CellValue errortype -> CellValue errortype)) |
  FuncValue Variable CellType (CellValue errortype) deriving Show

data ShowWrap w = ShowWrap String w

instance Show (ShowWrap w) where
  show (ShowWrap s _) = s

data CellValue errortype =
  RawValue (CellRawValue errortype)                  |
  RefValue CellIndex                                 |
  VariableValue Text                                 |
  NamedRefValue NamedReference                       |
  Range [CellIndex]                                  |
  App (CellValue errortype) (CellValue errortype) deriving Show
