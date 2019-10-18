module AST where

data Expr
  = VarExpr String
  | IntExpr Integer
  | LetExpr [(String, Expr)] Expr
  deriving (Show)
