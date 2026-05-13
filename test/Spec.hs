import Test.HUnit

import qualified TestParser
import qualified TestExpr
import qualified TestStatement
import qualified TestProgram

main :: IO ()
main = runTestTTAndExit $ TestList [
    TestLabel "Parser" TestParser.tests,
    TestLabel "Expressions" TestExpr.tests,
    TestLabel "Statements" TestStatement.tests,
    TestLabel "Programs" TestProgram.tests]
