module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token, newline, comment,
              spaces, word, (-#), (#-)) where

import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #-

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]
iter m = m # iter m >-> cons ! return []

cons(a, b) = a:b

-- Runs both parsers but only keep the result of second one
(-#) :: Parser a -> Parser b -> Parser b
m -# n = error "-# not implemented"

-- Runs both parsers but only keeps the result of the first
(#-) :: Parser a -> Parser b -> Parser a
m #- n = error "#- not implemented"

-- Here's an example of an implementation.
-- Treat comments as whitespace
-- >>> spaces "\t\t--comment\n"
-- Just ("\t\tcomment","")
-- >>> spaces "-- comment\n"
-- Just ("comment","")
-- >>> spaces "not space!"
-- Just ("","not space!")
spaces :: Parser String
-- spaces =  error "spaces not implemented"
spaces =
    -- Reads one whitespace character as a space and return something like [' ']
    let oneSpace = char ? (`elem` [' ', '\t', '\n']) >-> (:[])
    -- Reads many whitespace characters and concatenates them
    in iter (comment ! oneSpace) >-> concat

-- Treat comments as whitespace
-- >>> comment "--hello!\n"
-- Just ("hello!","")
-- >>> comment "--Not a comment"
-- Nothing
-- We include this because otherwise, it's quite tricky to support
comment :: Parser String
comment = dash -# dash -# notNewLine #- newline
    where notNewLine = iter (char ? (/= '\n'))
          dash = char ? (== '-')

-- Implement the rest here

token :: Parser a -> Parser a
token m = m #- spaces

letter :: Parser Char
letter =  error "letter not implemented"

word :: Parser String
word = token (letter # iter letter >-> cons)

chars :: Int -> Parser String
chars n =  error "chars not implemented"

accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

require :: String -> Parser String
require w  = error "require not implemented"

lit :: Char -> Parser Char
lit c = token char ? (==c)

newline :: Parser Char
newline = char ? (== '\n')

digit :: Parser Char
digit = char ? isDigit

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')

