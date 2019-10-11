module Main where

import Lib

-- Example:
-- let x = 1
--     y = let y = 1
--             z = 2
--          in z
--  in x

main :: IO ()
main = do
  expr <- readExpr "syntax.egi"
  print expr
