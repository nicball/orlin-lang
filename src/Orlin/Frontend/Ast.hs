module Orlin.Frontend.Ast (
    Prog (..)
  , Stmt (..)
  , Expr (..)
  , Type (..)
  , exprType
  , returnType
  , freeVariables
  , renameVariable
  ) where

newtype Prog = Prog [Stmt]

data Stmt
  = Def String Expr
  | Decl String Type

data Expr
  = Int Int
  | Var String Type Word
  | Unit
  | Fn [(String, Type)] [Expr]
  | Let (String, Expr) [Expr]
  | Call Expr [Expr] Type
  deriving (Eq)

data Type
  = TInt
  | TUnit
  | TFn [Type] Type
  deriving (Eq)

isVar :: Expr -> Bool
isVar (Var _ _ _) = True
isVar _ = False

varFromName :: String -> Expr
varFromName n = Var n TUnit 0

varName :: Expr -> String
varName (Var n _ _) = n

exprType :: Expr -> Type
exprType (Int _) = TInt
exprType (Var _ t _) = t
exprType Unit = TUnit
exprType (Fn params body) = TFn (map snd params) (exprType (last body))
exprType (Let _ body) = exprType (last body)
exprType (Call _ _ t) = t

freeVariables :: Expr -> [(String, Type, Word)]
freeVariables expr = iter 0 expr
  where iter _ (Int _) = []
        iter level (Var name t lvl) =
          if lvl >= level
            then [(name, t, lvl - level)]
            else []
        iter _ Unit = []
        iter level (Fn _ body) =
          concatMap (iter (level + 1)) body
        iter level (Let _ body) =
          concatMap (iter (level + 1)) body
        iter level (Call callee args _) =
          iter level callee ++ concatMap (iter level) args

renameVariable :: String -> String -> Expr -> Expr
renameVariable from to = iter 0
  where iter _ (Int n) = Int n
        iter level (Var n t lvl) =
          if lvl == level && n == from
            then (Var to t lvl)
            else (Var n t lvl)
        iter _ Unit = Unit
        iter level (Fn params body) =
          Fn params (map (iter (level + 1)) body)
        iter level (Let (name, value) body) =
          Let (name, iter level value) (map (iter (level + 1)) body)
        iter level (Call callee args t) =
          Call (iter level callee) (map (iter level) args) t


returnType :: Type -> Type
returnType (TFn _ t) = t

instance Show Prog where
  show (Prog defs) = unlines . map show $ defs

instance Show Stmt where
  show (Def name value) =
    "(define " ++ name ++ " " ++ show value ++ ")"
  show _ = ""

instance Show Expr where
  show (Int i) = show i ++ ":int"
  show (Var name t n) = name ++ "(" ++ show n ++ "):" ++ show t
  show Unit = "unit:unit"
  show e@(Fn params body) =
    "(fn [" ++ paramsStr ++ "]" ++ bodyStr ++ "):" ++ show (exprType e)
    where paramsStr = showParam (head params)
            ++ concatMap ((", " ++) . showParam) (tail params)
          showParam (name, t) = name ++ " " ++ show t
          bodyStr = concatMap ((" " ++) . show) body
  show e@(Let (name, value) body) =
    "(let [" ++ name ++ " " ++ show value ++ "]" ++ bodyStr ++ "):" ++ show (exprType e)
    where bodyStr = concatMap ((" " ++) . show) body
  show (Call callee args t) =
    "(" ++ show callee ++ concatMap ((" " ++) . show) args ++ "):" ++ show t

instance Show Type where
  show TInt = "int"
  show TUnit = "unit"
  show (TFn params ret) =
    "(-> " ++ ps ++ show ret ++ ")"
    where ps = concatMap (\t -> show t ++ " ") params
