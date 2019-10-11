module Lib where

import           Control.Monad.Identity  hiding (mapM)
import           Control.Applicative     ((*>), (<*), (<|>))
import           Text.Parsec             hiding ((<|>))
import           Text.Parsec.String
import qualified Text.Parsec.Token       as P
import qualified Text.Parsec.Indent      as Indent

egisonDef :: P.GenLanguageDef String () (Indent.IndentT Identity)
egisonDef =
  P.LanguageDef { P.commentStart       = "#|"
                , P.commentEnd         = "|#"
                , P.commentLine        = ";"
                , P.identStart         = letter
                , P.identLetter        = letter
                , P.opStart            = oneOf ":!#$%&*+./<=>?@\\^|-~"
                , P.opLetter           = oneOf ":!#$%&*+./<=>?@\\^|-~"
                , P.reservedNames      = ["let", "in"]
                , P.reservedOpNames    = ["=", "+"]
                , P.nestedComments     = True
                , P.caseSensitive      = True }

lexer :: P.GenTokenParser String () (Indent.IndentT Identity)
lexer = P.makeTokenParser egisonDef

reserved :: String -> Indent.IndentParser String () ()
reserved = P.reserved lexer

reservedOp :: String -> Indent.IndentParser String () ()
reservedOp = P.reservedOp lexer

data Expr
  = VarExpr String
  | IntExpr Integer
  | LetExpr [(String, Expr)] Expr
  deriving (Show)

pExpr = try pLetExpr
    <|> try pIntExpr
    <|> pVar

pVar :: Indent.IndentParser String () Expr
pVar = do
  x <- P.identifier lexer
  return $ VarExpr x

pIntExpr :: Indent.IndentParser String () Expr
pIntExpr = do
  x <- P.integer lexer
  return $ IntExpr x

pLetExpr :: Indent.IndentParser String () Expr
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

pLetExpr' :: Indent.IndentParser String () (String, Expr)
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
