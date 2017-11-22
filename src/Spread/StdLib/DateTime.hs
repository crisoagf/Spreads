{-# LANGUAGE OverloadedStrings #-}
module Spread.StdLib.DateTime where
import Spread.TypesAndVals
import Spread.StdLib
import Data.Time
import Data.Text.Buildable
import Morte.Core
import Data.Monoid

basicDateFns :: (Buildable i, Show i) => [(NamedReference, CellRawExpr i)]
basicDateFns = [("DateToTime", liftHaskFnDate "DateToTime" (Embed $ TypeValue Time) (\ _ i -> Right $ Embed $ TimeValue $ UTCTime i 0)),
                ("AddDays", liftHaskFnInt "AddDays" (Pi "" (Embed $ TypeValue Date) (Embed $ TypeValue Date)) (\ _ i -> Right $
                  liftHaskFnDate ("AddDays " <> pretty i) (Embed $ TypeValue Date) (\ _ d -> Right $ Embed $ DateValue $ addDays (fromIntegral i) d))),
                ("DiffDays", liftHaskFnDate "DiffDays" (Pi "" (Embed $ TypeValue Date) (Embed $ TypeValue Int)) (\ _ d1 -> Right $ 
                  liftHaskFnDate ("DiffDays " <> pretty d1) (Embed $ TypeValue Date) (\ _ d2 -> Right $ Embed $ IntValue (fromIntegral (diffDays d1 d2)))))
                ]


