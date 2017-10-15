module Spread.TypesAndVals where
import Numeric.Natural
import Data.Time
import Data.Time.Calendar
import Data.Text (Text)

type Variable = Text
type NamedReference = Text
type CellIndex = (Natural, Natural)

data CellType =
  IntType                    |
  FloatType                  |
  StringType                 |
  DateType                   |
  TimeType                   |
  ListType CellType          |
  FuncType CellType CellType deriving (Show, Eq)

data CellRawValue errortype =
  IntValue Int                          |
  FloatValue Double                     |
  StringValue Text                      |
  DateValue Day                         |
  TimeValue UTCTime                     |
  ListValue [CellRawValue errortype]              |
  LiftHaskFun CellType CellType (ShowWrap (
    (CellValue errortype -> Either errortype (CellType, CellRawValue errortype)) -> 
    CellValue errortype -> CellValue errortype)) |
  FuncValue Variable CellType (CellValue errortype) deriving Show

newtype ShowWrap w = ShowWrap w

instance Show (ShowWrap w) where
  show (ShowWrap _) = "(hidden)"

data CellValue errortype =
  RawValue (CellRawValue errortype)                  |
  VariableValue Text                                 |
  NamedRefValue NamedReference                       |
  Range [CellIndex]                                  |
  App (CellValue errortype) (CellValue errortype) deriving Show
