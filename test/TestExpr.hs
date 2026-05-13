module TestExpr (tests) where

import Test.HUnit

import qualified Dictionary
import Expr

-- Tests parsing
testPrintAndParse s = let e = fromString s :: Expr.T
       in s ~?= toString e

e1 = testPrintAndParse "1" 
e2 = testPrintAndParse "x" 
e3 = testPrintAndParse "x+y" 
e4 = testPrintAndParse "x-y-y" 
e21 = testPrintAndParse "1/(2-y)" 
e31 = testPrintAndParse "2+z" 

testsParsing = TestList [e1, e2, e3, e4, e21, e31]


-- A test environment
dict = Dictionary.insert ("x", 1) $
       Dictionary.insert ("y", 2) $
       Dictionary.empty 

testValue string = value (fromString string) dict

n1 = test $ testValue "1" ~?= (Right 1)
n2 = test $ testValue "x" ~?= (Right 1)
n3 = test $ testValue "x+y" ~?= (Right 3)
n4 = test $ testValue "x-y-y" ~?= (Right (-3))
n21 = test $ testValue "1/(2-y)" ~?= (Left "division by 0")
n31 = test $ testValue "2+z" ~?= (Left "undefined variable z")

testValue1 = TestList [n1, n2, n3, n4, n21, n31]

-- testing exponentiation

p1 = test $ testValue "y^3" ~?= (Right 8)
p2 = test $ testValue "y^3^4" ~?= (Right $ 2^3^4)
p3 = test $ testValue "8^4" ~?= (Right $ 8^4)
p4 = test $ testValue "(y+3)*2^(x+y)" ~?= (Right $ (2+3)*2^(1+2))
p5 = test $ testValue "2^3^4+2^5*6+7^8+9" ~?= (Right $ 2^3^4+2^5*6+7^8+9)

testValue2 = TestList [p1, p2, p3, p4, p5]

-- p1 = 8
-- p2 should be much larger than p3!
-- p2 = 2^81 = 2417851639229258349412352
-- p3 = 8^4 = 2^12 = 4096
-- p4 = 40
-- p5 = 2417851639229258355177354

tests = TestList [testsParsing, testValue1, testValue2]

