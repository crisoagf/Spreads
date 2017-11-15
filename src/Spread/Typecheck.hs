{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Spread.Typecheck where
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Spread.TypesAndVals
import Control.Arrow
import Morte.Core
import qualified Morte.Context as Ctx
import Control.Monad (join, forM, (>=>))
import Data.Set (Set)
import qualified Data.Set as S

data EvalContext = EvalContext { getEvalFn :: Either CellExpr CellIndex -> EvalM CellWithType, getNamedRefFn :: NamedReference -> Maybe CellRawExpr }
data InferContext = InferContext { getInferFn :: Either CellExpr CellIndex -> InferM CellType, inferNamedRefFn :: NamedReference -> Maybe CellRawExpr }

type EvalM  = ExceptT CellError (Reader EvalContext )
type InferM = ExceptT CellError (Reader InferContext)

inferToEval :: InferM a -> EvalM a
inferToEval = mapExceptT (withReader (\ ectx@(EvalContext evalFn nrs) -> InferContext (mapExceptT (pure . (`runReader` ectx)) . fmap cellType . evalFn) nrs))

runEval :: EvalM a -> EvalContext -> Either CellError a
runEval = runReader . runExceptT

typeAndEvaluate :: CellExpr -> EvalM CellWithType
typeAndEvaluate = fmap (uncurry CellWithType) . runKleisli (Kleisli (getType >>> inferToEval) &&& Kleisli evaluate)

getTypeExprX :: CellExpr -> InferM (Map (Either (OrdWrap CellType) Text) [(Int, CellValue)], Expr X)
getTypeExprX expr = do
  (exprMap, lifted) <- liftVals expr
  ctx <- giveContext exprMap
  (exprMap,) <$> liftMorte (typeWith ctx lifted)

getType :: CellExpr -> InferM CellType
getType = getTypeExprX >=> uncurry returnTypes

evaluate :: CellExpr -> EvalM CellRawExpr
evaluate = runKleisli $ Kleisli rawify >>> arr normalize >>> Kleisli normalizeHaskFns >>> Kleisli (\ (continue, x) -> if continue then evaluate (fmap RawValue x) else pure x)

liftMorte :: Either TypeError a -> InferM a
liftMorte = either (throwE . TE) pure

liftVals :: CellExpr -> InferM (Map (Either (OrdWrap CellType) Text) [(Int, CellValue)], Expr X)
liftVals expr = (fmap join . uncurry (flip (,))) <$> (runStateT (traverse (\ x -> case x of
  RawValue (TypeValue   t) -> pure (toPi t)
  RawValue (IntValue    _) -> addToMap x (Embed $ Int)
  RawValue (FloatValue  _) -> addToMap x (Embed $ Float)
  RawValue (StringValue _) -> addToMap x (Embed $ String)
  RawValue (DateValue   _) -> addToMap x (Embed $ Date)
  RawValue (TimeValue   _) -> addToMap x (Embed $ Time)
  RawValue (PosValue    _) -> addToMap x (Embed $ Pos)
  RawValue (ListValue   t _) -> addToMap x (App (Embed List) t)
  RawValue (LiftHaskFun tv t1 t2 _) -> addToMap x (Pi tv t1 t2)
  RefValue ci -> modify (M.insert (Right $ pretty x) [(0, x)]) >> pure (Var (V (pretty x) 0))
  NamedRefValue name -> modify (M.insert (Right $ pretty x) [(0, x)]) >> pure (Var (V (pretty x) 0))
  ) expr) M.empty)
  where addToMap :: CellValue -> CellType -> StateT (Map (Either (OrdWrap CellType) Text) [(Int, CellValue)]) InferM (Expr X)
        addToMap x t = do
                         (mPrevList, newMap) <- gets (M.insertLookupWithKey (\ _ [(_, v)] l -> (length l,v):l) (Left $ OrdWrap t) [(0, x)])
                         put newMap
                         pure (Var (V (T.pack $ show t ++ "V") (maybe 0 length mPrevList)))

giveContext :: Map (Either (OrdWrap CellType) Text) [(Int, CellValue)] -> InferM (Context (Expr X))
giveContext = M.foldrWithKey (\ ett l ctx -> case ett of
  Left (OrdWrap t) -> foldl' (\ ctx' (x, v) -> Ctx.insert (T.pack $ show t ++ "V") (toPi t) <$> ctx') ctx l
  Right name -> foldl' (\ ctx' (_, v) -> case v of
    x@(RefValue i) -> do
      inferFn <- lift (asks getInferFn)
      (Ctx.insert (pretty x) . toPi) <$> inferFn (Right i) <*> ctx'
    x@(NamedRefValue name) -> do
      inferFn <- lift (asks getInferFn)
      namedRefs <- lift (asks inferNamedRefFn)
      Ctx.insert (pretty x) <$> (maybe (throwE $ RE $ NamedRefDoesNotExist name) (fmap toPi . inferFn . Left . fmap RawValue) $ namedRefs name) <*> ctx') ctx l
    ) (pure basics)

basics :: Context (Expr X)
basics =
  Ctx.insert "Int" (Const Star) $
  Ctx.insert "Float" (Const Star) $
  Ctx.insert "String" (Const Star) $
  Ctx.insert "Date" (Const Star) $
  Ctx.insert "Time" (Const Star) $
  Ctx.insert "Pos" (Const Star) $
  Ctx.insert "List" (Pi "" (Const Star) (Const Star)) Ctx.empty

returnTypes :: Map (Either (OrdWrap CellType) Text) [(Int, CellValue)] -> Expr X -> InferM CellType
returnTypes m (Var (V x n)) =
  if x == "Int"    then pure $ Embed Int else
  if x == "Float"  then pure $ Embed Float else
  if x == "String" then pure $ Embed String else
  if x == "Date"   then pure $ Embed Date else
  if x == "Time"   then pure $ Embed Time else
  if x == "Pos"    then pure $ Embed Pos else
  if x == "List"   then pure $ Embed List else
  if x == "Const StarV"  then do
    inferFn <- lift (asks getInferFn)
    maybe (pure $ Var (V x n)) (\ (RawValue (TypeValue t)) -> pure t) (lookup n =<< M.lookup (Left (OrdWrap $ Const Star)) m) else pure $ Var (V x n)
returnTypes m (App x y) = App <$> returnTypes m x <*> returnTypes m y
returnTypes m (Pi x a b) = Pi x <$> returnTypes m a <*> returnTypes m b
returnTypes m (Lam x a b) = Lam x <$> returnTypes m a <*> returnTypes m b
returnTypes _ (Const y) = pure $ Const y

toPi :: CellType -> Expr X
toPi = ((\t -> Var (V (T.pack $ show t) 0)) =<<)

returnRawVals :: Map RawType [(Int, CellRawValue)] -> Expr X -> CellRawExpr
returnRawVals m expr = M.foldrWithKey (\ t l subexpr -> foldr (\ (x, rv) subexpr' -> subst (T.pack $ show t) x (Embed rv) subexpr') subexpr l) (liftExpr expr) m

liftExpr :: Expr X -> Expr a
liftExpr = fmap absurd

rawify :: CellExpr -> EvalM CellRawExpr
rawify expr = join <$> traverse rawify' expr

rawify' :: CellValue -> EvalM CellRawExpr
rawify' (RawValue x) = pure (Embed x)
rawify' (RefValue i) = join $ lift $ asks (fmap cellValue . ($ Right i) . getEvalFn)
rawify' (NamedRefValue r) = maybe (throwE $ RE $ NamedRefDoesNotExist r) pure =<< lift (asks (($ r) . getNamedRefFn))
--rawify' (Range t r) = (Embed . ListValue t) <$> join (lift $ asks (forM (fmap Right r) . getEvalFn))

normalizeHaskFns :: CellRawExpr -> EvalM (Bool, CellRawExpr)
normalizeHaskFns (App x y) = do
  (contx, x') <- normalizeHaskFns x
  (conty, y') <- normalizeHaskFns y
  evalFn <- lift $ asks getEvalFn
  evalSt <- lift $ ask
  pure $ case x' of
    Embed (LiftHaskFun tv t1 t2 (ShowWrap name f)) -> (True, f (flip runReader evalSt . runExceptT . evalFn) (RawValue <$> y'))
    _ -> (contx || conty, App x' y')
normalizeHaskFns x = pure (False, x)

