module Expr(Expr, T, parse, fromString, value, toString, Result) where

{-
   An expression of type Expr is a representation of an arithmetic expression 
   with integer constants and variables. A variable is a string of upper- 
   and lower case letters. The following functions are exported
   
   parse :: Parser Expr
   fromString :: String -> Expr
   toString :: Expr -> String
   value :: Expr -> Dictionary.T String Int -> Int
   
   parse is a parser for expressions as defined by the module Parser.
   It is suitable for use in parsers for languages containing expressions
   as a sublanguage.
   
   fromString expects its argument to contain an expression and returns the 
   corresponding Expr. 
  
   toString converts an expression to a string without unneccessary 
   parentheses and such that fromString (toString e) = e.
  
   value e env evaluates e in an environment env that is represented by a
   Dictionary.T Int.  
-}

import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary

data Expr = Num Integer | Var String | Add Expr Expr 
       | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Pow Expr Expr 
         deriving Show

type T = Expr

var, num, factor, term, expr :: Parser Expr

term', expr' :: Expr -> Parser Expr

var = word >-> Var

num = number >-> Num

mulOp = lit '*' >-> (\_ -> Mul) !
        lit '/' >-> (\_ -> Div)

addOp = lit '+' >-> (\_ -> Add) !
        lit '-' >-> (\_ -> Sub)

bldOp e (oper,e') = oper e e'

-- Val = primary 
val :: Parser Expr
val = num !
      var !
      lit '(' -# expr #- lit ')' !
      err "illegal value"

powOp = lit '^' >-> (\_ -> Pow)

-- Factor = val | val ^ factor
factor' e = powOp # factor >-> bldOp e #> factor' ! return e
factor = val #> factor'

-- Term = factor * factor
term' e = mulOp # factor >-> bldOp e #> term' ! return e
term = factor #> term'
       
-- Expr = term + term
expr' e = addOp # term >-> bldOp e #> expr' ! return e
expr = term #> expr'

parens cond str = if cond then "(" ++ str ++ ")" else str

shw :: Int -> Expr -> String
shw prec (Num n) = show n
shw prec (Var v) = v
shw prec (Add t u) = parens (prec>5) (shw 5 t ++ "+" ++ shw 5 u)
shw prec (Sub t u) = parens (prec>5) (shw 5 t ++ "-" ++ shw 6 u)
shw prec (Mul t u) = parens (prec>6) (shw 6 t ++ "*" ++ shw 6 u)
shw prec (Div t u) = parens (prec>6) (shw 6 t ++ "/" ++ shw 7 u)
shw prec (Pow t u) = parens (prec>6) (shw 6 t ++ "^" ++ shw 7 u)

type Result a = Either String a

safeDiv :: Integral b => b -> b -> Either String b
safeDiv _ 0 = Left "division by 0"
safeDiv x y = Right $ x `div` y


value :: Expr -> Dictionary.T String Integer -> Result Integer
value (Num n) _ = Right n
value (Var v) e = case Dictionary.lookup v e of
        Nothing -> Left $ "undefined variable " <> v
        Just val -> Right val
value (Add e1 e2) e = (+) <$> value e1 e <*> value e2 e
value (Sub e1 e2) e = (-) <$> value e1 e <*> value e2 e
value (Mul e1 e2) e = (*) <$> value e1 e <*> value e2 e
value (Div e1 e2) e = do
        x <- value e1 e
        y <- value e2 e 
        safeDiv x y 

value (Pow e1 e2) e = (^) <$> value e1 e <*> value e2 e


instance Parse Expr where
    parse = expr
    toString = shw 0
