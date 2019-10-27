{-# LANGUAGE TupleSections #-}

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
reserved s = P.reserved lexer s <* spaces

reservedOp :: String -> Parser ()
reservedOp s = P.reservedOp lexer s <* spaces

identifier :: Parser String
identifier = P.identifier lexer <* spaces

integerLiteral :: Parser Integer
integerLiteral = P.integer lexer <* spaces

pExpr :: Parser Expr
pExpr = pLetExpr
    <|> pAtomExpr

pAtomExpr :: Parser Expr
pAtomExpr = VarExpr <$> identifier
        <|> IntExpr <$> integerLiteral

pLetExpr :: Parser Expr
pLetExpr = Indent.withPos $ do
  reserved "let"
  bindings <- many $ Indent.indented *> pBinding
  reserved "in"
  expr <- pExpr
  return $ LetExpr bindings expr
  where
    pBinding :: Parser (String, Expr)
    pBinding = (,) <$> identifier <*> (reservedOp "=" >> pExpr)

readExpr :: FilePath -> IO Expr
readExpr filePath = do
    txt <- readFile filePath
    let result = Indent.runIndentParser parser () filePath txt
    case result of
        Left  err -> fail (show err)
        Right expr -> return expr
  where
    parser = pExpr <* eof
