{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Megaparsec where

import           Control.Monad (void)
import           Control.Monad.Combinators.Expr -- from parser-combinators
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           AST

type Parser = Parsec Void String

-- space comsumer
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.decimal

reserved :: String -> Parser ()
reserved w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [String] -- list of reserved words
rws = ["let", "in"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

pExpr :: Parser Expr
pExpr = pLetExpr
    <|> pIntExpr
    <|> pVar

pIntExpr :: Parser Expr
pIntExpr = IntExpr <$> integer

pVar :: Parser Expr
pVar = VarExpr <$> identifier

pLetExpr :: Parser Expr
pLetExpr = do
  pos <- L.indentLevel
  reserved "let"
  bindings <- some (L.indentGuard sc GT pos *> pLetExpr')
  reserved "in"
  expr <- pExpr
  return $ LetExpr bindings expr

pLetExpr' :: Parser (String, Expr)
pLetExpr' = do
  x <- identifier
  symbol "="
  expr <- pExpr
  return (x, expr)

readExpr :: FilePath -> IO Expr
readExpr filePath = do
    txt <- readFile filePath
    let result = parse parser filePath txt
    case result of
        Left  err -> fail (show err)
        Right expr -> return expr
  where
    parser = pExpr <* eof
