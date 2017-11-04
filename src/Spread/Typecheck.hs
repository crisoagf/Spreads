{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Spread.Typecheck where
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Spread.TypesAndVals
import Data.Map (Map)
import Data.Char
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Monad
import Data.Foldable
import Data.Bifunctor
import Debug.Trace

type EvalContext = (Map TypeVariable CellType, Map NamedReference (CellValue TypeError))

type EvalM = ExceptT TypeError (State EvalContext)
type InferM = ExceptT TypeError (State (Map TypeVariable CellType))

inferToEval :: InferM a -> EvalM a
inferToEval ia = do
  tvs <- lift $ gets fst
  mapExceptT ((\ (a, newtvs) -> modify (first (const newtvs)) >> pure a) . (`runState` tvs)) ia

resultToEval :: Applicative m => Either TypeError a -> ExceptT TypeError m a
resultToEval = ExceptT . pure

data TypeError =
  FreeVar Variable                   |
  FuncBodyDoesNotTypecheck TypeError |
  AppWithoutFunc                     |
  AppMismatch TypeError (CellValue TypeError) |
  NoSuchName                         |
  VarTypeMismatch                    |
  ValTypeMismatch                    |
  NonHomogeneousList                 |
  InferEmptyList                     |
  TypeMismatch CellType CellType     |
  RefError TypeError                 |
  GrossMismatch (CellValue TypeError) deriving Show

typeMatch :: CellType -> CellType -> InferM ()
typeMatch (VarType v) (VarType w) = maybe (maybe (pure ()) (\ x -> (lift $ modify (M.insert v x))) =<< lift (gets (M.lookup w))) (\ x -> maybe (pure ()) (typeMatch x) =<< lift (gets (M.lookup w))) =<< lift (gets (M.lookup v))
typeMatch (VarType v)  x          = lift (gets (M.lookup v)) >>= maybe (lift $ modify (M.insert v (fresh x))) (typeMatch x)
typeMatch x           (VarType v) = lift (gets (M.lookup v)) >>= maybe (lift $ modify (M.insert v (fresh x))) (typeMatch x)
typeMatch (FuncType x x') (FuncType y y') = typeMatch x y >> typeMatch x' y'
typeMatch (ListType x) (ListType y) = typeMatch x y
typeMatch x y = if x == y then pure () else throwE (TypeMismatch x y)

fresh :: CellType -> CellType
fresh (VarType v) = VarType (T.toTitle v)
fresh (FuncType x x') = FuncType (fresh x) (fresh x')
fresh (ListType x) = ListType (fresh x)
fresh x = x

evaluate :: (CellIndex -> Either TypeError (CellType, CellRawValue TypeError)) -> CellValue TypeError -> EvalM (CellType, CellRawValue TypeError) 
evaluate evalFn val = evaluate' evalFn val {- do
  (cellType, value) <- evaluate' evalFn val
  (cellType,) <$> releaseVars (evaluate' evalFn) value -}

releaseVars :: (CellValue TypeError -> EvalM (CellType, CellRawValue TypeError)) -> CellRawValue TypeError -> EvalM (CellRawValue TypeError)
releaseVars evalFn value = do
  (refsThatAreVars, releasedRefs) <- lift $ gets (first M.toList . M.partitionWithKey (\ k _ -> maybe False (isLower . fst) (T.uncons k)) . snd)
  newVal <- foldrM (\ (refName, refVal) oldVal -> do
    (evType, evVal) <- evalFn refVal
    --traceM ("Releasing " ++ show refName)
    lift $ modify (second (M.insert refName (RawValue $ evVal)))
    pure (replaceExpr' refName (RawValue evVal) oldVal)) value refsThatAreVars
  lift $ modify (second (const releasedRefs))
  pure newVal

evaluate' :: (CellIndex -> Either TypeError (CellType, CellRawValue TypeError)) -> CellValue TypeError -> EvalM (CellType, CellRawValue TypeError) 
evaluate' _    (RawValue (IntValue    x            )) = pure (IntType   , IntValue    x)
evaluate' _    (RawValue (FloatValue  x            )) = pure (FloatType , FloatValue  x)
evaluate' _    (RawValue (StringValue x            )) = pure (StringType, StringValue x)
evaluate' _    (RawValue (DateValue   x            )) = pure (DateType  , DateValue   x)
evaluate' _    (RawValue (TimeValue   x            )) = pure (TimeType  , TimeValue   x)
evaluate' _    (RawValue (PosValue    x            )) = pure (PosType   , PosValue    x)
evaluate' _    (RawValue (TypeValue   x            )) = pure (TypeType  , TypeValue   x)
evaluate' vals (RawValue (ListValue   x            )) = bimap ListType ListValue <$> do
  vs <-  mapM (evaluate vals . RawValue) x
  case vs of
    [] -> throwE InferEmptyList
    (a:as) -> foldrM (\ (nextType, nextVal) (prevType, prevValList) ->
      if nextType /= prevType
      then throwE NonHomogeneousList
      else pure (prevType, nextVal : prevValList)) (second (const []) a) (a:as)
evaluate' vals (RawValue (FuncValue var varType val)) =
  do
    (tvs, names) <- lift $ get
    toType <- flip catchE (throwE . FuncBodyDoesNotTypecheck) $ inferToEval $ inferType (M.singleton var varType) names (fmap fst . vals) val
    pure (FuncType varType toType, FuncValue var varType val)
evaluate' vals (RawValue (LiftHaskFun fromType toType x)) = pure (FuncType fromType toType, LiftHaskFun fromType toType x)
evaluate' vals (RefValue refName) = flip catchE (throwE . RefError) $ evaluate vals . RawValue . snd =<< resultToEval (vals refName)
evaluate' vals (NamedRefValue refName) = lift (gets snd) >>= \ names -> maybe (throwE NoSuchName) (evaluate vals) (M.lookup refName names)
evaluate' vals (Range indList) = bimap ListType ListValue <$> ((\case
  [] -> throwE InferEmptyList
  (a:as) -> foldrM (\ (nextType, nextVal) (prevType, prevValList) -> if nextType /= prevType then throwE NonHomogeneousList else pure (prevType, nextVal : prevValList)) (second (const []) a) (a:as)) =<< mapM (evaluate vals . RawValue . snd <=< resultToEval . vals) indList)
evaluate' vals expr@(App (RawValue (FuncValue var varType val)) x) =
  do
    names <- lift (gets snd)
    replacingType <- inferToEval $ inferType M.empty names (fmap fst . vals) x
    bodyType <- flip catchE (throwE . FuncBodyDoesNotTypecheck) $ inferToEval $ inferType (M.singleton var varType) names (fmap fst . vals) val
    inferToEval $ typeMatch replacingType varType
    newtvs <- lift (gets fst)
    lift $ modify (second (M.insert var x))
    evaluate vals (replaceExpr var (NamedRefValue var) val)
evaluate' vals expr@(App (RawValue (LiftHaskFun varType valType (ShowWrap s f))) x) = do
  names <- lift (gets snd)
  replacingType <- inferToEval $ inferType M.empty names (fmap fst . vals) x
  inferToEval $ typeMatch replacingType varType
  newtvs <- lift (gets fst)
  lift $ modify (first (const newtvs))
  evaluate vals (f (either ((`evalState` (newtvs, names)) . runExceptT . evaluate vals) vals) x)
evaluate' vals (App (NamedRefValue refName) x) = maybe (throwE NoSuchName) (\ new -> evaluate vals (App new x)) =<< lift (gets (M.lookup refName . snd))
evaluate' vals (App (RefValue refName) x) = flip catchE (throwE . RefError) $ (\ new -> evaluate vals (App new x)) . RawValue . snd =<< resultToEval (vals refName)
evaluate' vals (App (App x y) z) = (\ (newType, new) -> evaluate vals (App (RawValue new) z)) =<< evaluate vals (App x y)
evaluate' _ (App _ _) = throwE AppWithoutFunc
evaluate' _ (VariableValue v) = throwE (FreeVar v)
--evaluate' _ expr = throwE (GrossMismatch expr)

inferType :: Map Variable CellType -> Map NamedReference (CellValue TypeError) -> (CellIndex -> Either TypeError CellType) -> (CellValue TypeError) -> InferM CellType
inferType _      _      cellMap (RawValue (IntValue    _            )) = pure IntType
inferType _      _      cellMap (RawValue (FloatValue  _            )) = pure FloatType
inferType _      _      cellMap (RawValue (StringValue _            )) = pure StringType
inferType _      _      cellMap (RawValue (DateValue   _            )) = pure DateType
inferType _      _      cellMap (RawValue (TimeValue   _            )) = pure TimeType
inferType _      _      cellMap (RawValue (PosValue    x            )) = pure PosType
inferType _      _      cellMap (RawValue (TypeValue   x            )) = pure TypeType
inferType varMap refMap cellMap (RawValue (ListValue   x            )) = ListType <$> ((\case
  [] -> throwE InferEmptyList
  (t:ts) -> foldr (\ nextType ePrevType -> (\ prevType -> if prevType == nextType then pure prevType else throwE NonHomogeneousList) =<< ePrevType) (pure t) ts) =<< mapM (inferType varMap refMap cellMap . RawValue) x)
inferType _ _ _ (RawValue (LiftHaskFun fromType toType _)) = pure (FuncType fromType toType)
inferType varMap refMap cellMap (RawValue (FuncValue var varType val)) = FuncType varType <$> inferType (M.insert var varType varMap) refMap cellMap val
inferType varMap refMap cellMap (NamedRefValue refName) = maybe (throwE NoSuchName) (inferType varMap refMap cellMap) (M.lookup refName refMap)
--inferType _ _ _ cellMap (Range indList) = ListType <$> ((\case
--  [] -> Left InferEmptyList
--  (t:ts) -> foldr (\ nextType ePrevType -> do
--    prevType <- ePrevType
--    when (prevType /= nextType) (Left NonHomogeneousList)
--    ePrevType) (pure t) ts) =<< mapM cellMap indList)
inferType varMap refMap cellMap expr@(App x y) = do
  (\case
    FuncType t1 t2 -> do
      ty <- inferType varMap refMap cellMap y
      flip catchE (throwE . (`AppMismatch` expr)) $ typeMatch ty t1
      pure t2
    _ -> throwE AppWithoutFunc) =<< inferType varMap refMap cellMap x
inferType _ _ cellMap (RefValue refName) = resultToEval $ first RefError $ cellMap refName
inferType varMap _ _ (VariableValue varName) = maybe (throwE $ FreeVar varName) pure (M.lookup varName varMap)
--inferType _ _ _ expr = Left (GrossMismatch expr)

replaceExpr :: Variable -> (CellValue TypeError) -> (CellValue TypeError) -> (CellValue TypeError)
replaceExpr name newVal orig@(VariableValue name') = if name == name' then newVal else orig
replaceExpr name newVal orig@(RawValue (FuncValue var varType val)) = if name == var then orig else RawValue (FuncValue var varType (replaceExpr name newVal val))
replaceExpr name newVal (App x y) = App (replaceExpr name newVal x) (replaceExpr name newVal y)
replaceExpr _    _       x        = x

replaceRef :: NamedReference -> (CellValue TypeError) -> (CellValue TypeError) -> (CellValue TypeError)
replaceRef name newVal orig@(NamedRefValue name') = if name == name' then newVal else orig
replaceRef name newVal orig@(RawValue (FuncValue var varType val)) = RawValue (FuncValue var varType (replaceRef name newVal val))
replaceRef name newVal (App x y) = App (replaceRef name newVal x) (replaceRef name newVal y)
replaceRef _    _       x        = x

replaceExpr' :: NamedReference -> (CellValue TypeError) -> (CellRawValue TypeError) -> (CellRawValue TypeError)
replaceExpr' name newVal orig@(FuncValue var varType val) = FuncValue var varType (replaceRef name newVal val)
replaceExpr' _    _       x        = x

