module Main where

import Indents    as I
import Megaparsec as M

-- Example:
-- let x = 1
--     y = let y = 1
--             z = 2
--          in z
--  in x

main :: IO ()
main = do
  expr <- I.readExpr "syntax.egi"
  print expr
