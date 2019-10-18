module Indents where

import           Control.Monad.Identity  hiding (mapM)
import           Control.Applicative     ((*>), (<*), (<|>))
import           Text.Parsec             hiding ((<|>))
import           Text.Parsec.String      hiding (Parser)
import qualified Text.Parsec.Token       as P
import qualified Text.Parsec.Indent      as Indent

import           AST

type Parser a = Indent.IndentParser String () a

languageDef :: P.GenLanguageDef String () (Indent.IndentT Identity)
languageDef =
  P.LanguageDef { P.commentStart       = "{-"
                , P.commentEnd         = "-}"
                , P.commentLine        = "--"
                , P.identStart         = letter
                , P.identLetter        = letter
                , P.opStart            = oneOf ":!#$%&*+./<=>?@\\^|-~"
                , P.opLetter           = oneOf ":!#$%&*+./<=>?@\\^|-~"
                , P.reservedNames      = ["let", "in"]
                , P.reservedOpNames    = ["=", "+"]
                , P.nestedComments     = True
                , P.caseSensitive      = True }

lexer :: P.GenTokenParser String () (Indent.IndentT Identity)
lexer = P.makeTokenParser languageDef

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

pExpr :: Parser Expr
pExpr = try pLetExpr
    <|> try pIntExpr
    <|> pVar

pVar :: Parser Expr
pVar = do
  x <- P.identifier lexer
  return $ VarExpr x

pIntExpr :: Parser Expr
pIntExpr = do
  x <- P.integer lexer
  return $ IntExpr x

pLetExpr :: Parser Expr
pLetExpr = Indent.withPos $ do
  reserved "let"
  spaces
  binding <- pLetExpr'
  spaces
  bindings' <- many $ Indent.indented *> pLetExpr'
  spaces
  reserved "in"
  spaces
  expr <- pExpr
  return $ LetExpr (binding : bindings') expr

pLetExpr' :: Parser (String, Expr)
pLetExpr' = do
  x <- P.identifier lexer
  spaces
  reservedOp "="
  spaces
  expr <- pExpr
  return (x, expr)

readExpr :: FilePath -> IO Expr
readExpr filePath = do
    txt <- readFile filePath
    let result = Indent.runIndentParser parser () filePath txt
    case result of
        Left  err -> fail (show err)
        Right expr -> return expr
  where
    parser = pExpr <* eof
