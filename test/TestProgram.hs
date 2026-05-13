{- Test for Program -}
module TestProgram (tests) where

import Test.HUnit

import Program

p0, p1, p2 :: Program.T

-- basic program, no comments

s0 = "\
\read k;\
\read n;\
\m := 1;\
\while n-m do\
\  begin\
\    if m - m/k*k then\
\      skip;\
\    else\
\      write m;\
\  m := m + 1;\
\  end"

p0 = fromString s0

-- Printing and reading should yield the same results
-- (Should not be sensitive to whitespace)
testp0 :: Test
testp0 = TestLabel "p0" $ p0 ~=? fromString (toString p0)

-- does execution work as expected?

testExecp0 :: Test
testExecp0 = TestLabel "p0" $ Program.exec p0 [3, 16] ~?= [3, 6, 9, 12, 15]

-- this time some comments and exponents

s1 = "\
\read a;\
\read b;\
\-- a comment\n\
\s := 3;\
\while a do\
\  begin\
\    c := a^s;\
\    d := 2^a;\
\    write c;\
\    write d;\
\    a := a-1;\
\  end\
\write a;"

p1 = fromString s1

testp1 :: Test
testp1 = TestLabel "p1" $ fromString (toString p1) ~=? p1

testExecp1 :: Test
testExecp1 = TestLabel "p1" $ Program.exec p1 [4, 4] ~?= [64, 16, 27, 8, 8, 4, 1, 2, 0]

p2 = fromString ("\
\begin\
\  read n;   -- just reading n...\n\
\  -- this line should just be ignored\n\
\  fac := -- initialize fac\n\
\         -- to 1:\n\
\         1;\
\  while n do\
\    begin\
\      fac := fac*n;\
\      n := n-1;\
\    end\
\  write -- woops\n\
\  fac;\
\end")

testp2 :: Test
testp2 = TestLabel "p2" $ fromString (toString p2) ~=? p2

testExecp2 = TestLabel "p2" $ Program.exec p2 [5] ~?= [120]

testsPrinting = TestLabel "Printing" $ TestList [testp0, testp1, testp2]

testsExecution = TestLabel "Execution" $ TestList [testExecp0, testExecp1, testExecp2]

tests = TestList [testsPrinting, testsExecution]