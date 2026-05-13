{- Test for Parser.hs -}
module TestParser (tests) where

import Test.HUnit

import Prelude hiding (return, fail)
import Parser

l1 = test $ letter "abc" ~?= Just ('a', "bc")  
l2 = test $ letter "123" ~?= Nothing   
l3 = test $ letter "" ~?= Nothing      

testLetters = TestList [l1, l2, l3]

w1 = test $ spaces "abc" ~?= Just ("", "abc")
w2 = test $ spaces "  \t abc"  ~?= Just("  \t ","abc")

testSpaces = TestList [w1, w2]

c1 = test $ chars 2 "abc" ~?= Just ("ab","c")
c2 = test $ chars 0 "ab"  ~?= Just ("","ab")
c3 = test $ chars 3 "ab" ~?= Nothing

testChars = TestList [c1, c2, c3]

r1 = test $ require ":=" ":= 1" ~?= Just (":=","1")
r2 = test $ require "else" "then" ~?= Nothing -- Program error: expecting else near then

testRequire = TestList [r1, r2]

a4 = test $ (accept "read" -# word) "read count" ~?= Just ("count","")

tests = TestList [testLetters, testSpaces, testChars, testRequire, a4]