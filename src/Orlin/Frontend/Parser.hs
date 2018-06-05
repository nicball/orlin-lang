module Orlin.Frontend.Parser (
    program
  ) where

import Text.Parsec 
import Text.Parsec.String (Parser)
import Orlin.Frontend.Ast 

program :: Parser Prog
program = Prog <$> many1 statement <* spaces

statement :: Parser Stmt
statement = try definition <|> declaration

definition :: Parser Stmt
definition = namedParens "var" $ do
  name <- varName <$> variable
  value <- expression
  return $ Def name value

declaration :: Parser Stmt
declaration = namedParens ":" $ do
  name <- varName <$> variable
  ty <- type_
  return $ Decl name ty

expression :: Parser Expr
expression = try number
         <|> try unit
         <|> try variable
         <|> try fn
         <|> try let_
         <|> call

number :: Parser Expr
number = spaces *> (Int . read <$> many1 digit)

variable :: Parser Expr
variable = do
  spaces
  first <- letter <|> oneOf "+-*/!?_"
  rest <- many (alphaNum <|> oneOf "+-*/!?_'")
  return $ Var (first:rest) TUnit 0

unit :: Parser Expr
unit = wsString ":unit" *> pure Unit

fn :: Parser Expr
fn = namedParens "fn" $
  Fn <$> parens (many1 param)
     <*> many1 expression
  where param = do
          name <- varName <$> variable
          ty <- type_
          return (name, ty)

let_ :: Parser Expr
let_ = namedParens "let" $ do
  binds <- parens (many1 binding)
  body <- many1 expression
  return $ foldr (\b e -> Let b [e])
                 (Let (last binds) body)
                 (init binds)
  where binding = do
          name <- varName <$> variable
          value <- expression
          return (name, value)

call :: Parser Expr
call = parens $
  Call <$> callee
       <*> many1 expression
       <*> pure TUnit
  where callee = do
          e <- expression
          if isVar e && (varName e `elem` ["let", "fn"])
            then unexpected (varName e ++ ": Bad syntax.")
            else return e

type_ :: Parser Type
type_ = try (wsString "int" *> return TInt)
    <|> try (wsString "unit" *> return TUnit)
    <|> arrow
  where arrow = namedParens "->" $ do
          first <- type_
          rest <- many1 type_
          return $ TFn (first : init rest) (last rest)

parens :: Parser a -> Parser a
parens p = lparen *> p <* rparen

namedParens :: String -> Parser a -> Parser a
namedParens name p = lparen *> wsString name *> p <* rparen

lparen :: Parser Char
lparen = spaces *> oneOf "(["

rparen :: Parser Char
rparen = spaces *> oneOf ")]"

wsString :: String -> Parser String
wsString s = spaces *> string s
