{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Spread.Typecheck (EvalM, InferM, EvalContext (..), InferContext (..), SomeM, getType, typeAndEvaluate, evaluate, cellDeps, runSome) where
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Bifunctor
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Spread.TypesAndVals
import Control.Arrow hiding (first, second)
import Data.Bifunctor (first)
import Morte.Core
import qualified Morte.Context as Ctx
import Control.Monad (join)
import Data.Set (Set)
import qualified Data.Set as S
import Control.Applicative ((<|>))
import Debug.Trace

data EvalContext i = EvalContext {getEvalFn :: i -> EvalM i (CellWithType i), getNamedRefFn :: NamedReference -> Maybe (CellRawExpr i)}
data InferContext i = InferContext {getInferFn :: i -> InferM i CellType, inferNamedRefFn :: NamedReference -> Maybe (CellRawExpr i)}

type EvalM  i = SomeM (EvalContext i) i
type InferM i = SomeM (InferContext i) i
type SomeM r i = ExceptT (CellError i) (Reader r)

asksL :: (r -> d) -> SomeM r i d
asksL = lift . asks

cellDeps :: Ord a => CellExpr a -> Set a
cellDeps = foldr (\case
  RawValue (PosValue a) -> S.insert a
  RefValue a -> S.insert a
  _ -> id) S.empty

inferToEval :: InferM i a -> EvalM i a
inferToEval = mapExceptT (withReader (\ ectx@(EvalContext evalFn nrs) -> InferContext (mapExceptT (pure . (`runReader` ectx)) . fmap cellType . evalFn) nrs))

runSome :: r -> SomeM r i a -> Either (CellError i) a
runSome r = flip runReader r . runExceptT

typeAndEvaluate :: (Ord i, Show i) => i -> CellExpr i -> EvalM i (CellWithType i)
typeAndEvaluate ind = fmap (uncurry CellWithType) . runKleisli (Kleisli (getType ind >>> inferToEval) &&& Kleisli (evaluate ind))

getTypeExprX :: (Ord i, Show i) => i -> CellExpr i -> InferM i (Expr X)
getTypeExprX ind expr = do
  (ctx, lifted) <- liftVals ind expr
  liftMorte (typeWith ctx lifted)

getType :: (Ord i, Show i) => i -> CellExpr i -> InferM i CellType
getType ind = getTypeExprX ind >>> fmap returnTypes

evaluate :: i -> CellExpr i -> EvalM i (CellRawExpr i)
evaluate ind = runKleisli $ Kleisli rawify >>> arr normalize >>> normaliseInnermostHaskFns ^>> Kleisli (either (evaluate ind) pure)

liftMorte :: Either TypeError a -> SomeM r i a
liftMorte = liftEither . first TE

liftEither :: Either (CellError i) a -> SomeM r i a
liftEither = either throwE pure

liftVals :: (Ord i, Show i) => i -> CellExpr i -> InferM i (Context (Expr X), Expr X)
liftVals ind' expr = uncurry (flip (,)) <$> runStateT (liftVals' ind' expr) basics
  where liftVals' ind = fmap join . 
          traverse (\ x -> case x of
            RawValue (TypeValue   t) -> pure (Var (V (T.pack $ show t) 0))
            RawValue (IntValue    _) -> addToMap "Int"
            RawValue (FloatValue  _) -> addToMap "Float"
            RawValue (StringValue _) -> addToMap "String"
            RawValue (DateValue   _) -> addToMap "Date"
            RawValue (TimeValue   _) -> addToMap "Time"
            RawValue (PosValue    _) -> addToMap "Pos"
            RawValue (TimeDiffValue   _) -> addToMap "TimeDiff"
            RawValue (LiftHaskFun tv t1 t2 (ShowWrap s _)) -> do
              fnType <- (Pi tv <$> liftVals' ind (RawValue <$> t1) <*> liftVals' ind (RawValue <$> t2))
              modify (Ctx.insert (T.pack s) fnType)
              pure (Var (V (T.pack s) 0))
            ListValue t elems -> listValueOf <$> liftVals' ind t <*> traverse (liftVals' ind) elems
            RefValue i -> do
              inferFn <- lift $ asksL getInferFn
              ctx <- get
              newCtx <- lift (catchE ((flip (Ctx.insert (T.pack $ show x)) ctx . toPi) <$> inferFn i) (throwE . RE . ErrorInRef i))
              modify (const newCtx)
              pure (Var (V (T.pack $ show x) 0))
            NamedRefValue name -> do
              namedRefExpr <- lift (asksL (($ name) . inferNamedRefFn))
              maybe (lift (throwE (RE (NamedRefDoesNotExist name)))) (liftVals' ind . fmap RawValue) namedRefExpr)
        addToMap :: Text -> StateT (Context (Expr X)) (InferM i) (Expr X)
        addToMap t = do
          modify (Ctx.insert (t <> "V") (Var (V t 0)))
          pure (Var (V (t <> "V") 0)) -- In terms of type, I don't care...

basics :: Context (Expr X)
basics =
  Ctx.insert "Int" (Const Star) $
  Ctx.insert "Float" (Const Star) $
  Ctx.insert "String" (Const Star) $
  Ctx.insert "Date" (Const Star) $
  Ctx.insert "Time" (Const Star) $
  Ctx.insert "TimeDiff" (Const Star) $
  Ctx.insert "Pos" (Const Star) $
    Ctx.empty

returnTypes :: Expr X -> CellType
returnTypes (Var (V x n)) =
  if x == "Int"      then Embed Int else
  if x == "Float"    then Embed Float else
  if x == "String"   then Embed String else
  if x == "Date"     then Embed Date else
  if x == "Time"     then Embed Time else
  if x == "TimeDiff" then Embed TimeDiff else
  if x == "Pos"      then Embed Pos else Var (V x n)
returnTypes (App x y) = App (returnTypes x) (returnTypes y)
returnTypes (Pi x a b) = Pi x (returnTypes a) (returnTypes b)
returnTypes (Lam x a b) = Lam x (returnTypes a) (returnTypes b)
returnTypes (Const y) = Const y
returnTypes (Embed x) = absurd x

toPi :: CellType -> Expr X
toPi = ((\t -> Var (V (T.pack $ show t) 0)) =<<)

rawify :: CellExpr i -> EvalM i (CellRawExpr i)
rawify expr = join <$> traverse rawify' expr

rawify' :: CellValue i -> EvalM i (CellRawExpr i)
rawify' (RawValue x) = pure (Embed x)
rawify' (RefValue i) = join $ asksL (fmap cellValue . ($ i) . getEvalFn)
rawify' (NamedRefValue r) = maybe (throwE $ RE $ NamedRefDoesNotExist r) pure =<< asksL (($ r) . getNamedRefFn)
rawify' (ListValue t r) = listValueOf <$> rawify t <*> traverse rawify r

normaliseInnermostHaskFns :: CellRawExpr i -> Either (CellExpr i) (CellRawExpr i)
normaliseInnermostHaskFns (App x y) = 
        case normaliseInnermostHaskFns x of
          Left x' -> Left $ App x' $ either id (fmap RawValue) $ normaliseInnermostHaskFns y
          Right (Embed (LiftHaskFun _ _ _ (ShowWrap _ f))) -> Left $ either (App (fmap RawValue x)) f $ normaliseInnermostHaskFns y
          Right x' -> bimap (App (fmap RawValue x')) (App x') (normaliseInnermostHaskFns y)
normaliseInnermostHaskFns x = Right x

