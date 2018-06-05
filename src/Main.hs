module Main where

import System.IO
import Text.Parsec
import Control.Monad.Except
import qualified Data.ByteString as BS
import qualified LLVM.General.Module as L
import qualified LLVM.General.Context as L
import qualified LLVM.General.Target as L
import qualified LLVM.General.AST as AST

import Orlin.Frontend.Parser
import Orlin.Frontend.TypeChecker
import Orlin.Frontend.LinearIR
import Orlin.Frontend.CodeGen

main :: IO ()
main = do
  prog <- getContents
  case (parse program "<stdin>" prog) of
    Right ast ->
      case typeCheck ast of
        Right tast -> dumpObject (codegen . translate $ tast)
        Left err -> putStrLn err
    Left msg -> putStrLn . show $ msg

dumpObject :: AST.Module -> IO ()
dumpObject ast = L.withContext $ \ctx -> do
  res <- runExceptT . L.withModuleFromAST ctx ast $ \mod ->
    runExceptT . L.withHostTargetMachine $ \tm ->
      runExceptT $ L.moduleObject tm mod
  case res of
    Left err -> hPutStrLn stderr err
    Right (Left err) -> hPutStrLn stderr err
    Right (Right (Left err)) -> hPutStrLn stderr err
    Right (Right (Right obj)) -> BS.putStr obj
