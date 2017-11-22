{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Spreads.Vision where
import Spreads.Basic
data VisionToken = Input | Type | Value | Pres deriving Eq

data Vision (a :: VisionToken) where
  VisionInput :: Vision 'Input
  VisionType  :: Vision 'Type
  VisionValue :: Vision 'Value
  VisionPres  :: Vision 'Pres

type family VisionType (a :: VisionToken) where
  VisionType 'Input = Either CellError CellExpr
  VisionType 'Type  = Either CellError CellType
  VisionType 'Value = Either CellError CellWithType
  VisionType 'Pres  = () -- Not defined yet...

notRelevant :: (forall a . Vision a -> b) -> VisionToken -> b
notRelevant f Input = f VisionInput
notRelevant f Type  = f VisionType
notRelevant f Value = f VisionValue
notRelevant f Pres  = f VisionPres

visionToToken :: Vision a -> VisionToken
visionToToken VisionInput = Input
visionToToken VisionType  = Type
visionToToken VisionValue = Value
visionToToken VisionPres  = Pres

tokenToKnownVision :: VisionToken -> (forall a . Vision a -> Maybe (Vision a))
tokenToKnownVision a = \b -> if visionToToken b == a then Just b else Nothing

