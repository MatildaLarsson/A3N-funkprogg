module Program(T, parse, fromString, toString, exec) where

import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)

newtype T = Program [Statement.T]

instance Eq T where
  (Program stmts1) == (Program stmts2) = stmts1 == stmts2

instance Show T where
  show = toString

instance Parse T where
  parse = iter Statement.parse >-> Program
  toString (Program stmts) = concatMap Statement.toString stmts

exec :: T -> [Integer] -> [Integer]
exec (Program stmts) input = Statement.execute stmts Dictionary.empty input
