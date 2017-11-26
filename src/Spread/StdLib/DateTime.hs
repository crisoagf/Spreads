{-# LANGUAGE OverloadedStrings #-}
module Spread.StdLib.DateTime where
import Spread.TypesAndVals
import Spread.StdLib
import Data.Time
import Data.Text.Buildable
import Morte.Core
import Data.Monoid

basicDateFns :: (Buildable i, Show i) => [(NamedReference, CellRawExpr i)]
basicDateFns = [("DateToTime", liftHaskFnDate "DateToTime" (Embed $ TypeValue Time) (\ i -> Embed $ RawValue $ TimeValue $ UTCTime i 0)),
                ("AddDays", liftHaskFnInt "AddDays" (Pi "" (Embed $ TypeValue Date) (Embed $ TypeValue Date)) (\ i -> fmap RawValue $ liftHaskFnDate ("AddDays " <> pretty i) (Embed $ TypeValue Date) (\ d -> Embed $ RawValue $ DateValue $ addDays (fromIntegral i) d))),
                ("DiffDays", liftHaskFnDate "DiffDays" (Pi "" (Embed $ TypeValue Date) (Embed $ TypeValue Int)) (\ d1 -> fmap RawValue $ liftHaskFnDate ("DiffDays " <> pretty d1) (Embed $ TypeValue Date) (\ d2 -> Embed $ RawValue $ IntValue (fromIntegral (diffDays d1 d2)))))
                ]


