{-# LANGUAGE RankNTypes #-}
module LensCheats where
import Control.Lens

using :: Getter s a -> Lens s s b c -> Lens s s (a, b) c
using la lb f s = lb (curry f $ getConst $ la (\x -> Const x) s) s

getToGetter :: Getter s (a -> b) -> Getter s a -> Getter s b
getToGetter gf ga = to (\ s -> (view gf s) (view ga s))

applying :: (a -> b) -> Getter s a -> Getter s b
applying f ga bfb s = ga (contramap f . bfb . f) s

