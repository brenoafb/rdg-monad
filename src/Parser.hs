module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import TinyPL

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "if"
                                     , "else"
                                     , "while"
                                     , "func"
                                     , "return"
                                     ]
           , Token.reservedOpNames = [ "+", "-", "*", "/"
                                     , "="
                                     ]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
int = fromIntegral <$> Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer
braces = Token.braces lexer
comma = Token.comma lexer

program :: Parser Program
program = whiteSpace >> many function

function :: Parser Function
function = do
  reserved "func"
  name <- identifier
  args <- parens (identifier `sepBy` comma)
  body <- statement
  return $ Function name args body

statement :: Parser Stmt
statement = block <|> assignment <|> ifElseStmt
          <|> ifStmt <|> while <|>  returnStmt

block :: Parser Stmt
block = Block <$> braces (many statement)

assignment :: Parser Stmt
assignment = do
    var <- identifier
    reservedOp "="
    e <- expr
    semi
    return $ Assignment var e

ifElseStmt :: Parser Stmt
ifElseStmt = do
  reserved "if"
  cond <- parens expr
  conseq <- statement
  reserved "else"
  alt <- statement
  return $ IfElse cond conseq alt

ifStmt :: Parser Stmt
ifStmt = do
  reserved "if"
  cond <- parens expr
  stmt <- statement
  return $ If cond stmt

while :: Parser Stmt
while = do
  reserved "while"
  cond <- parens expr
  body <- statement
  return $ While cond body

returnStmt :: Parser Stmt
returnStmt = do
  reserved "return"
  e <- expr
  semi
  return $ Return e

expr :: Parser LExpr
expr = try funCall <|> buildExpressionParser operators term

funCall :: Parser LExpr
funCall = do
  funName <- identifier
  args <- parens (sepBy expr comma)
  return $ FunCall funName args

operators = [ [Prefix (reservedOp "-" >> return Neg)]
            , [Infix  (reservedOp "*" >> return Mult) AssocLeft,
               Infix  (reservedOp "/" >> return Div) AssocLeft]
            , [Infix  (reservedOp "+" >> return Add) AssocLeft,
               Infix  (reservedOp "-" >> return Sub) AssocLeft]
            ]

term = parens expr
     <|>  Var <$> identifier
     <|> Num <$> int

parseStr :: String -> Program
parseStr str = case parse program "" str of
                 Left e -> error $ show e
                 Right r -> r
