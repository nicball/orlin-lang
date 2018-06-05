module Orlin.Frontend.CodeGen (
    codegen
  ) where

import Control.Monad.State
import Data.Word
import qualified Data.Map as Map
import qualified LLVM.General.AST.Global as Global
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Type as AST
import qualified LLVM.General.AST.Constant as Constant
import qualified LLVM.General.AST.Attribute as Attribute
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import qualified Orlin.Frontend.LinearIR as IR

codegen :: IR.Prog -> AST.Module
codegen prog = execModuleGen (genProg prog) "main"

genProg :: IR.Prog -> ModuleGen ()
genProg (IR.Prog defs) = do
  defineFunction "malloc" [("size", AST.i32)] (AST.ptr AST.i8) []
  mapM_ genDef defs

genDef :: IR.GlobalDef -> ModuleGen ()
genDef (IR.FunctionDef name capTys params retTy body) = do
  defineFunction name params' (fromIRType retTy) blks
  where params' = ("_Env", AST.ptr AST.i8) : map (\(n, t) -> ("_A" ++ n, fromIRType t)) params
        blks = getBasicBlocks . execFuncGen $ do
          addBlock "entry" >>= setCurrentBlockName
          let envTy = envType capTys
          env <- bitcast (localref (AST.ptr AST.i8) "_Env") (AST.ptr envTy)
          modify $ \s -> s { currentEnv = env }
          forM_ params $ \(name, t) -> do
            p <- alloca name (fromIRType t)
            store p (localref (fromIRType t) ("_A" ++ name))
          genBlock body >>= ret
genDef (IR.VariableDef name value) = do
  defineVariable name
                 (fromIRType . IR.exprType . IR.ConstantExpr $ value)
                 (fromIRConstant value)

genBlock :: [IR.Expr] -> FuncGen AST.Operand
genBlock exprs = do
  values <- mapM genExpr exprs
  return (last values)

genExpr :: IR.Expr -> FuncGen AST.Operand
genExpr (IR.ConstantExpr c) = return . AST.ConstantOperand . fromIRConstant $ c
genExpr (IR.LetExpr name t value body) = do
  var <- alloca name (fromIRType t)
  val <- genExpr value
  store var val
  genBlock body
genExpr e@(IR.RefExpr ref) = do
  ptr <- fromIRRef ref
  let t = fromIRType . IR.exprType $ e
  load t ptr
genExpr (IR.CallExpr (IR.MakeClosureExpr name t@(IR.TFunc argTys retTy) _) args) = do
  let fn = globalref (fromIRType t) name
      env = AST.ConstantOperand (Constant.Null (AST.ptr AST.i8))
  args' <- mapM genExpr args
  call (fromIRType retTy) fn (env:args')
genExpr e@(IR.CallExpr callee args) = do
  callee' <- genExpr callee
  let (IR.TClosure argTys retTy) = IR.exprType callee
      fnTy = fromIRType (IR.TFunc argTys retTy)
  fn <- extractvalue (AST.ptr fnTy) callee' [0]
  env <- extractvalue (AST.ptr AST.i8) callee' [1]
  args' <- mapM genExpr args
  let t = fromIRType . IR.exprType $ e
  call t fn (env:args')
genExpr (IR.MakeClosureExpr name t@(IR.TFunc args ret) caps) = do
  let fn = globalref (fromIRType t) name
  captures <- mapM (genExpr . IR.RefExpr) caps
  let mallocTy = AST.FunctionType (AST.ptr AST.i8) [AST.i32] False
      malloc = globalref (AST.ptr mallocTy) "malloc"
      capTys = map (IR.exprType . IR.RefExpr) caps
      clsTy = closureType args ret
      fnptrTy = AST.ptr (fromIRType t)
      envTy = envType capTys
      envSize = structSize envTy
      cls = structConstant [Constant.Null fnptrTy, Constant.Null (AST.ptr AST.i8)]
  cls <- insertvalue clsTy cls fn [0]
  env <- if null caps
    then return $ AST.ConstantOperand (Constant.Null (AST.ptr AST.i8))
    else do
      mem <- call (AST.ptr AST.i8) malloc [envSize]
      caps <- bitcast mem (AST.ptr envTy)
      forM_ (zip3 [0..] captures capTys) $ \(idx, op, t) -> do
        var <- getelementptr (AST.ptr . fromIRType $ t) caps [0, idx]
        store var op
      return mem
  cls <- insertvalue clsTy cls env [1]
  return cls

closureType :: [IR.Type] -> IR.Type -> AST.Type
closureType argTys retTy =
  AST.StructureType False [
      AST.ptr (AST.FunctionType (fromIRType retTy) ((AST.ptr AST.i8) : (map fromIRType argTys)) False)
    , AST.ptr AST.i8
    ]

envType :: [IR.Type] -> AST.Type
envType capTys =
  AST.StructureType False (map fromIRType capTys)

fromIRType :: IR.Type -> AST.Type
fromIRType IR.TInt = AST.i64
fromIRType IR.TUnit = AST.ptr AST.i8
fromIRType (IR.TFunc params ret)
  = AST.FunctionType (fromIRType ret) ((AST.ptr AST.i8) : (map fromIRType params)) False
fromIRType (IR.TClosure args ret) = closureType args ret

fromIRConstant :: IR.Constant -> Constant.Constant
fromIRConstant (IR.ConstantInt n) = Constant.Int 64 (fromIntegral n)
fromIRConstant IR.ConstantUnit = Constant.Null (AST.ptr AST.i8)

fromIRRef :: IR.Ref -> FuncGen AST.Operand
fromIRRef (IR.GlobalRef name t)
  = return $ globalref (fromIRType t) name
fromIRRef (IR.LocalRef name t)
  = return $ localref (AST.ptr . fromIRType $ t) name
fromIRRef (IR.CaptureRef idx t) = do
  cap <- gets currentEnv
  var <- getelementptr (AST.ptr (fromIRType t)) cap [0, idx]
  return var

type ModuleGen = State AST.Module

execModuleGen :: ModuleGen a -> String -> AST.Module
execModuleGen m name = execState m emptyModule
  where emptyModule = AST.defaultModule { AST.moduleName = name }

defineFunction :: String 
               -> [(String, AST.Type)] 
               -> AST.Type
               -> [AST.BasicBlock]
               -> ModuleGen ()
defineFunction nm params retTy body = addDefinition . AST.GlobalDefinition $
  Global.functionDefaults {
    Global.name = AST.Name nm
  , Global.parameters = ps
  , Global.returnType = retTy
  , Global.basicBlocks = body
  }
  where ps = ( [ Global.Parameter t (AST.Name n) []
               | (n, t) <- params
               ]
             , False
             )

defineVariable :: String -> AST.Type -> Constant.Constant -> ModuleGen ()
defineVariable nm t init = addDefinition . AST.GlobalDefinition $
  Global.globalVariableDefaults {
    Global.name = AST.Name nm
  , Global.type' = t
  , Global.initializer = Just init
  }

addDefinition :: AST.Definition -> ModuleGen ()
addDefinition d = do
  defs <- gets AST.moduleDefinitions
  modify $ \s ->
    s { AST.moduleDefinitions = defs ++ [d] }

data FuncGenState
  = FuncGenState {
    currentEnv :: AST.Operand
  , currentBlockName :: AST.Name
  , blocks :: [(AST.Name, BlockState)]
  , nameSupply :: Map.Map String Word
  , unNameSupply :: Word
  }

type FuncGen = State FuncGenState

execFuncGen :: FuncGen a -> FuncGenState
execFuncGen m = execState m emptyFuncGen
  where emptyFuncGen = FuncGenState (AST.LocalReference AST.void (AST.Name "NONEXISTEDENV")) 
                                    (AST.Name "")
                                    [] 
                                    Map.empty 
                                    0

getBasicBlocks :: FuncGenState -> [AST.BasicBlock]
getBasicBlocks s = map mkBlock (blocks s)
  where mkBlock (n, BlockState b t) = AST.BasicBlock n b t

uniqueName :: String -> FuncGen String
uniqueName name = do
  nms <- gets nameSupply
  case Map.lookup name nms of
    Nothing -> do
      modify $ \s -> s
        { nameSupply = Map.insert name 1 nms }
      return (name ++ show 0)
    Just i -> do
      modify $ \s -> s
        { nameSupply = Map.insert name (i+1) nms }
      return (name ++ show i)

uniqueUnName :: FuncGen Word
uniqueUnName = do
  c <- gets unNameSupply
  modify $ \s -> s { unNameSupply = c + 1 }
  return c

data BlockState
  = BlockState {
    body :: [AST.Named AST.Instruction]
  , terminator :: AST.Named AST.Terminator
  }

currentBlock :: FuncGen BlockState
currentBlock = do
  n <- gets currentBlockName
  blks <- gets blocks
  case lookup n blks of
    Just b -> return b
    Nothing -> error "currentBlock: internal error."

modifyCurrentBlock :: BlockState -> FuncGen ()
modifyCurrentBlock bs = do
  name <- gets currentBlockName
  blks <- gets blocks
  let (before, (_:after)) = span (\(n, _) -> n /= name) blks
  modify $ \s -> s
    { blocks = before ++ ((name, bs):after) }

addBlock :: String -> FuncGen AST.Name
addBlock n = do
  blks <- gets blocks
  name <- uniqueName n
  modify $ \s -> s {
      blocks = blks ++ [(AST.Name name, BlockState [] (AST.Do (AST.Ret Nothing [])))]
    }
  return (AST.Name name)

setCurrentBlockName :: AST.Name -> FuncGen ()
setCurrentBlockName n = do
  modify $ \s -> s
    { currentBlockName = n }

insertInst :: AST.Type -> AST.Instruction -> FuncGen AST.Operand
insertInst t i = do
  ix <- uniqueUnName
  let ref = AST.UnName ix
  blk <- currentBlock
  let ins = body blk
  modifyCurrentBlock (blk { body = ins ++ [ref AST.:= i] })
  return (AST.LocalReference t ref)

insertNamedInst :: String -> AST.Type -> AST.Instruction -> FuncGen AST.Operand
insertNamedInst n t i = do
  let ref = AST.Name n
  blk <- currentBlock
  let ins = body blk
  modifyCurrentBlock (blk { body = ins ++ [ref AST.:= i] })
  return (AST.LocalReference t ref)

setTerminator :: AST.Named AST.Terminator -> FuncGen ()
setTerminator term = do
  blk <- currentBlock
  modifyCurrentBlock (blk { terminator = term })

localref :: AST.Type -> String -> AST.Operand
localref t name = AST.LocalReference t (AST.Name name)

globalref :: AST.Type -> String -> AST.Operand
globalref t name = AST.ConstantOperand (Constant.GlobalReference t (AST.Name name))

alloca :: String -> AST.Type -> FuncGen AST.Operand
alloca n t = insertNamedInst n (AST.ptr t) $ AST.Alloca t Nothing 0 []

store :: AST.Operand -> AST.Operand -> FuncGen AST.Operand
store ptr val = insertInst AST.void $ AST.Store False ptr val Nothing 0 []

load :: AST.Type -> AST.Operand -> FuncGen AST.Operand
load t ptr = insertInst t $ AST.Load False ptr Nothing 0 []

call :: AST.Type -> AST.Operand -> [AST.Operand] -> FuncGen AST.Operand
call t fn args = insertInst t $ AST.Call Nothing CC.C [] (Right fn) (toArgs args) [] []
  where toArgs = map (\a -> (a, []))

extractvalue :: AST.Type -> AST.Operand -> [Word32] -> FuncGen AST.Operand
extractvalue t agg ixs = insertInst t $ AST.ExtractValue agg ixs []

insertvalue :: AST.Type -> AST.Operand -> AST.Operand -> [Word32] -> FuncGen AST.Operand
insertvalue t agg v ixs = insertInst t $ AST.InsertValue agg v ixs []

bitcast :: AST.Operand -> AST.Type -> FuncGen AST.Operand
bitcast val t = insertInst t $ AST.BitCast val t []

getelementptr :: AST.Type -> AST.Operand -> [Word] -> FuncGen AST.Operand
getelementptr t ptr ixs = insertInst t $ AST.GetElementPtr True ptr (map mkConst ixs) []
  where mkConst i = AST.ConstantOperand (constantWord i)

ret :: AST.Operand -> FuncGen ()
ret op = setTerminator (AST.Do (AST.Ret (Just op) []))

structConstant :: [Constant.Constant] -> AST.Operand
structConstant flds = AST.ConstantOperand (Constant.Struct Nothing False flds)

structSize :: AST.Type -> AST.Operand
structSize t =
  let nullptr = Constant.Null (AST.ptr t)
      nextElem = Constant.GetElementPtr False nullptr [constantWord 1]
      size = Constant.PtrToInt nextElem AST.i32
  in AST.ConstantOperand size

constantWord :: Word -> Constant.Constant
constantWord n = Constant.Int 32 (fromIntegral n)
