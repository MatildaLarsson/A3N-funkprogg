module Statement(T, parse, toString, fromString, execute) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr

type T = Statement
data Statement =
    Assignment String Expr.T 
    | If Expr.T Statement Statement
    | Skip
    | Begin [Statement]
    | While Expr.T Statement
    | Read String
    | Write Expr.T
    deriving (Show, Eq)

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> uncurry Assignment

skip = accept "skip" -# require ";"
       >-> \_ -> Skip

begin = accept "begin" -# iter parse #- require "end"
        >-> Begin

iff = accept "if" -# Expr.parse # require "then" -# parse
      # (require "else" -# parse)
      >-> \((cond, thenS), elseS) -> If cond thenS elseS

while = accept "while" -# Expr.parse # (require "do" -# parse)
        >-> uncurry While

read_ = accept "read" -# word #- require ";"
        >-> Read

write = accept "write" -# Expr.parse #- require ";"
        >-> Write

class Executable t where
    execute :: [t] -> Dictionary.T String Integer -> [Integer] -> [Integer]

instance Executable Statement where
    -- execute :: [Statement] -> Dictionary.T String Integer -> [Integer] -> [Integer]
    execute [] _ _ = []

    execute (Assignment v e : stmts) dict input = 
        case Expr.value e dict of
            Left msg -> error msg
            Right val -> execute stmts (Dictionary.insert (v, val) dict) input
    
    execute (Skip : stmts) dict input = 
        execute stmts dict input

    execute (Begin block : stmts) dict input = 
        execute (block ++ stmts) dict input

    execute (If cond thenStmts elseStmts: stmts) dict input =
        case (Expr.value cond dict) of
            Left msg -> error msg
            Right v ->
                if v > 0 then
                    execute (thenStmts: stmts) dict input
                else
                    execute (elseStmts: stmts) dict input
    execute (While cond body : stmts) dict input =
        case Expr.value cond dict of
            Left msg -> error msg
            Right v ->
                if v > 0
                    then execute (body : While cond body : stmts) dict input
                    else execute stmts dict input

    execute (Read v : stmts) dict (n:input) = 
        execute stmts (Dictionary.insert (v,n) dict) input

    execute (Read _ : _) _ [] = error "read: no input"

    execute (Write e : stmts) dict input = 
        case Expr.value e dict of
            Left msg -> error msg
            Right val -> val : execute stmts dict input

instance Parse Statement where
  parse = assignment ! skip ! begin ! iff ! while ! read_ ! write
  toString (Assignment v e)  = v ++ " := " ++ Expr.toString e ++ ";"

  toString Skip               = "skip;"

  toString (Begin stmts)     = "begin " ++ concatMap toString stmts ++ " end"

  toString (If c t e)        = "if " ++ Expr.toString c ++ " then " ++ toString t ++ " else " ++ toString e

  toString (While c body)    = "while " ++ Expr.toString c ++ " do " ++ toString body

  toString (Read v)          = "read " ++ v ++ ";"

  toString (Write e)         = "write " ++ Expr.toString e ++ ";"
