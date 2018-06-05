module Orlin.Frontend.TypeChecker (
    typeCheck
  ) where

import Control.Monad.RWS
import Orlin.Frontend.Ast

typeCheck :: Prog -> Either String Prog
typeCheck prog =
  if msgs == []
    then Right prog'
    else Left (unlines msgs)
  where (prog', msgs) = 
          evalRWS (checkProg prog) () [[]]

type TypeChecker = RWS () [String] Symtab
type Symtab = [[(String, Type)]]

checkProg :: Prog -> TypeChecker Prog
checkProg (Prog prog) = do
    p <- mapM checkStmt prog
    let prog' = filter isDef p
    [st] <- get
    forM_ st $ \(name, _) -> do
      case lookupDef name prog' of
        Just _ -> return ()
        Nothing -> tell ["Variable \""
          ++ name
          ++ "\" declared but not defined."]
    return (Prog prog')
    where isDef (Def _ _) = True
          isDef _ = False
          lookupDef name =
            lookup name . map (\(Def n v) -> (n, v))

checkStmt :: Stmt -> TypeChecker Stmt
checkStmt (Def name value) = do
  if not (isConst value)
    then tell [name ++ " must be initialized by a constant."]
    else return ()
  value' <- checkExpr value
  st <- getActive
  case lookup name st of
    Just t ->
      if t /= exprType value'
        then typeError value' t (exprType value')
        else return ()
    Nothing -> return ()
  putSymbol (name, (exprType value'))
  return (Def name value')
  where isConst (Int _) = True
        isConst Unit = True
        isConst (Fn _ _) = True
        isConst _ = False
checkStmt (Decl name t) = do
  putSymbol (name, t)
  return (Decl name t)

checkExpr :: Expr -> TypeChecker Expr
checkExpr (Int i) = return (Int i)
checkExpr (Var name _ _) = do
  info <- lookupSymbol name
  case info of
    Just (ty, lvl) -> return (Var name ty lvl)
    Nothing -> undefinedError name 
      >> return (Var name TUnit 0)
checkExpr Unit = return Unit
checkExpr (Fn params body) = withScope $ do
  mapM_ putSymbol params
  body' <- mapM checkExpr body
  return (Fn params body')
checkExpr (Let (name, value) body) = do
  value' <- checkExpr value
  withScope $ do
    putSymbol (name, exprType value')
    body' <- mapM checkExpr body
    return (Let (name, value') body')
checkExpr (Call callee args _) = do
  callee' <- checkExpr callee
  args' <- mapM checkExpr args
  let checkParams (p:ps) (a:as) = do
        if p == exprType a
          then checkParams ps as
          else typeError a p (exprType a)
      checkParams [] (_:_) =
        tell ["Too many arguments for " ++ show callee']
      checkParams (_:_) [] =
        tell ["Too few arguments for " ++ show callee']
      checkParams [] [] = return ()
  if isFunc (exprType callee')
    then do
      let (TFn params ret) = exprType callee'
      checkParams params args'
      return (Call callee' args' ret)
    else
      return (Call callee' args' TUnit)
  where isFunc (TFn _ _) = True
        isFunc _ = False

typeError :: Expr -> Type -> Type -> TypeChecker ()
typeError expr expected got =
  tell [("At expression \"" ++ show expr ++ "\"\n"
    ++ "\tExpected type: " ++ show expected
    ++ "\n\tActual type: " ++ show got)]

undefinedError :: String -> TypeChecker ()
undefinedError name =
  tell [("Undefined variable \"" ++ name ++ "\".")]

getActive :: TypeChecker [(String, Type)]
getActive = do
  re <- gets head
  return re

withScope :: TypeChecker a -> TypeChecker a
withScope action = do
  enter
  re <- action
  leave
  return re

putSymbol :: (String, Type) -> TypeChecker ()
putSymbol nt@(name, _) = do
  (act:rest) <- get
  case lookup name act of
    Just _ -> tell ["Redefinition of variable \"" ++ name ++ "\"."]
    Nothing -> return ()
  put ((nt:act):rest)

popSymbol :: TypeChecker ()
popSymbol = do
  ((_:act):rest) <- get
  put (act:rest)

lookupSymbol :: String -> TypeChecker (Maybe (Type, Word))
lookupSymbol name = do
  st <- get
  return (iter st name 0)
  where iter (st:rest) name level =
          case lookup name st of
            Just t -> Just (t, level)
            Nothing -> iter rest name (level + 1)
        iter [] _ _ = Nothing

enter :: TypeChecker ()
enter = do
  modify ([]:)

leave :: TypeChecker ()
leave = do
  modify tail
