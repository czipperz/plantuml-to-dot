module ParseTests (parseTests) where

import Test.HUnit
import Parse
import Grammar
import TestUtil
import qualified Text.Parsec as Parsec

parseTests = concat [generalTests, arrowTests, nodeTests]

parse = parsePlantUML "*test*"

generalTests = [parseEmpty]
parseEmpty = assertEqTest "parseEmpty" (Right []) $ parse ""

arrowTests = [parseRightDependencyArrow, parseImplementsArrow]
parseRightDependencyArrow = assertEqTest "parseRightDependencyArrow"
  (Right [Edge (EdgeEnd "a" "") "--" (EdgeEnd "b" ">") ""]) $ parse "a --> b"
parseImplementsArrow = assertEqTest "parseImplementsArrow"
  (Right [Edge (EdgeEnd "a" "<|") ".." (EdgeEnd "b" "") ""]) $ parse "a <|.. b"

nodeTests = [parseClassNoBody, parseClassWithEmptyBody, parseClassWithField,
             parseClassWithMethod, parseClassWithFieldAndMethod]
parseClassNoBody = assertEqTest "parseClassNoBody"
  (Right [Node Class "AB.CD" [] []]) $ parse "class AB.CD"
parseClassWithEmptyBody = assertEqTest "parseClassWithEmptyBody"
  (Right [Node Class "AB.CD" [] []]) $ parse "class AB.CD {\n}\n"

parseClassWithField = assertEqTest "parseClassWithField"
  (Right [Node Class "AB.CD" [NodeElement "+a: int"] []])
  $ parse "class AB.CD {\n+a: int\n}\n"
parseClassWithMethod = assertEqTest "parseClassWithMethod"
  (Right [Node Class "AB.CD" [] [NodeElement "+f(a: double, b: int): double"]])
  $ parse "class AB.CD {\n+f(a: double, b: int): double\n}\n"
parseClassWithFieldAndMethod = assertEqTest "parseClassWithFieldAndMethod"
  (Right [Node Class "ABC" [NodeElement "+a: int"] [NodeElement "+f()"]])
  $ parse "class ABC {\n+a: int\n+f()\n}\n"
