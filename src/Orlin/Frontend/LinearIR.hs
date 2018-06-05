module Orlin.Frontend.LinearIR (
    Prog (..)
  , GlobalDef (..)
  , Type (..)
  , Expr (..)
  , Ref (..)
  , Constant (..)
  , translate
  , exprType
  , returnType
  ) where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import qualified Orlin.Frontend.Ast as Ast

data Prog = Prog [GlobalDef]
  deriving (Show)

data GlobalDef
  = FunctionDef String [Type] [(String, Type)] Type [Expr]
  | VariableDef String Constant
  deriving (Show)

data Type
  = TInt
  | TUnit
  | TFunc [Type] Type
  | TClosure [Type] Type
  deriving (Show)

data Expr
  = ConstantExpr Constant
  | LetExpr String Type Expr [Expr]
  | RefExpr Ref
  | CallExpr Expr [Expr]
  | MakeClosureExpr String Type [Ref]
  deriving (Show)

data Ref
  = GlobalRef String Type
  | LocalRef String Type
  | CaptureRef Word Type
  deriving (Show)

data Constant
  = ConstantInt Int
  | ConstantUnit
  deriving (Show)

exprType :: Expr -> Type
exprType (ConstantExpr (ConstantInt _)) = TInt
exprType (ConstantExpr ConstantUnit) = TUnit
exprType (LetExpr _ _ _ body) = exprType (last body)
exprType (RefExpr (GlobalRef _ t)) = t
exprType (RefExpr (LocalRef _ t)) = t
exprType (RefExpr (CaptureRef _ t)) = t
exprType (CallExpr e _) = returnType (exprType e)
exprType (MakeClosureExpr _ (TFunc args ret) _) = TClosure args ret

returnType :: Type -> Type
returnType (TFunc _ ret) = ret
returnType (TClosure _ ret) = ret

type Trans = State TransState

data TransState
  = TransState {
    level :: Word
  , letLevel :: Word
  , captures :: [(String, Word)]
  , lambdas :: [GlobalDef]
  , lamNameSupply :: Word
  , localNameSupply :: Map.Map String Word
  }

emptyTransState :: TransState
emptyTransState = TransState 0 0 [] [] 0 Map.empty

translate :: Ast.Prog -> Prog
translate prog = evalState (transProg prog) emptyTransState

transProg :: Ast.Prog -> Trans Prog
transProg (Ast.Prog defs) = do
  funcs <- mapM transDef defs
  lams <- gets lambdas
  return $ Prog (funcs ++ lams)

transDef :: Ast.Stmt -> Trans GlobalDef
transDef (Ast.Def name (Ast.Fn params body)) = do
  body' <- withLevel (mapM transExpr body)
  let params' = map (\(n, t) -> (n, fromAstType t)) params
      retTy = fromAstType (Ast.exprType (last body))
  return $ FunctionDef (mangle name) [] params' retTy body'
transDef (Ast.Def name (Ast.Int n)) =
  return $ VariableDef (mangle name) (ConstantInt n)
transDef (Ast.Def name Ast.Unit) =
  return $ VariableDef (mangle name) ConstantUnit

transExpr :: Ast.Expr -> Trans Expr
transExpr (Ast.Int i) = return . ConstantExpr . ConstantInt $ i
transExpr (Ast.Var name t lvl) = do
  curr <- gets level
  currLet <- gets letLevel
  if lvl == curr
    then return (globalFuncRef name t)
    else
      if lvl <= currLet
        then return . RefExpr $ LocalRef name (fromAstType t)
        else do
          n <- lookupCaptureExn name
          return . RefExpr $ CaptureRef n (fromAstType t)
  where globalFuncRef name t@(Ast.TFn _ _) =
          MakeClosureExpr (mangle name) (fromAstTypeGlobal t) []
        globalFuncRef name t = RefExpr (GlobalRef (mangle name) (fromAstTypeGlobal t))
transExpr Ast.Unit = return . ConstantExpr $ ConstantUnit
transExpr lam@(Ast.Fn params body) = do
  clearNameSupply
  let fvs = Ast.freeVariables lam
  curr <- gets level
  let caps = filter (\(_, _, lvl) -> lvl /= curr) fvs
      capsTy = map (\(_, t, _) -> fromAstType t) caps
      capsNm = map (\(n, _, _) -> n) caps
      params' = map (\(n, t) -> (n, fromAstType t)) params
      retTy = fromAstType . Ast.returnType . Ast.exprType $ lam
  mapM_ uniqueName capsNm
  lamNm <- genLamName
  capsRef <- mapM mkCapRef caps
  body' <- withCaptures (zip capsNm [0..]) $ do
    withLevel (mapM transExpr body)
  addLambda $ FunctionDef lamNm capsTy params' retTy body'
  return $ MakeClosureExpr lamNm (TFunc (map snd params') retTy)
                           capsRef
  where mkCapRef (n, t, lvl) = do
          letlvl <- gets letLevel
          if lvl <= letlvl
            then return $ LocalRef n (fromAstType t)
            else do
              ix <- lookupCaptureExn n
              return $ CaptureRef ix (fromAstType t)
transExpr (Ast.Let (name, value) body) = do
  name' <- uniqueName name
  let body' = map (Ast.renameVariable name name') body
  tval <- transExpr value
  tbody <- withLetLevel (mapM transExpr body')
  return $ LetExpr name' (fromAstType (Ast.exprType value)) (boxGlobalFunc tval) tbody
transExpr (Ast.Call callee args _) = do
  callee' <- transExpr callee
  args' <- mapM (liftM boxGlobalFunc . transExpr) args
  return $ CallExpr callee' args'

boxGlobalFunc :: Expr -> Expr
boxGlobalFunc (RefExpr (GlobalRef name t@(TFunc _ _))) =
  MakeClosureExpr name t []
boxGlobalFunc e = e

lookupCaptureExn :: String -> Trans Word
lookupCaptureExn name = do
  caps <- gets captures
  case lookup name caps of
    Just n -> return n
    Nothing -> error ("lookupCaptureExn: error looking up \"" ++ name ++ "\".")

genLamName :: Trans String
genLamName = do
  n <- gets lamNameSupply
  modify $ \s -> s { lamNameSupply = n + 1 }
  return ("lambda_" ++ show n)

addLambda :: GlobalDef -> Trans ()
addLambda def = do
  modify $ \s -> s { lambdas = lambdas s ++ [def] }

withCaptures :: [(String, Word)] -> Trans a -> Trans a
withCaptures caps action = do
  old <- gets captures
  modify $ \s -> s { captures = caps }
  re <- action
  modify $ \s -> s { captures = old }
  return re

withLevel :: Trans a -> Trans a
withLevel action = do
  oldlet <- gets letLevel
  modify $ \s -> s {
      level = level s + 1
    , letLevel = 0
    }
  re <- action
  modify $ \s -> s {
      level = level s - 1
    , letLevel = oldlet
    }
  return re

withLetLevel :: Trans a -> Trans a
withLetLevel action = do
  modify $ \s -> s {
      level = level s + 1
    , letLevel = letLevel s + 1
  }
  re <- action
  modify $ \s -> s {
      level = level s - 1
    , letLevel = letLevel s - 1
  }
  return re
  
uniqueName :: String -> Trans String
uniqueName name = do
  nms <- gets localNameSupply
  case Map.lookup name nms of
    Nothing -> do
      modify $ \s -> s
        { localNameSupply = Map.insert name 1 nms }
      return name
    Just i -> do
      modify $ \s -> s
        { localNameSupply = Map.insert name (i+1) nms }
      qname <- uniqueName (name ++ show i)
      return qname

clearNameSupply :: Trans ()
clearNameSupply = modify $ \s -> s { localNameSupply = Map.empty }

mangle :: String -> String
mangle s = '_' : s

fromAstType :: Ast.Type -> Type
fromAstType Ast.TInt = TInt
fromAstType Ast.TUnit = TUnit
fromAstType (Ast.TFn args ret) =
  TClosure (map fromAstType args) (fromAstType ret)

fromAstTypeGlobal :: Ast.Type -> Type
fromAstTypeGlobal Ast.TInt = TInt
fromAstTypeGlobal Ast.TUnit = TUnit
fromAstTypeGlobal (Ast.TFn args ret) =
  TFunc (map fromAstType args) (fromAstType ret)
  
