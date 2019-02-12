module TestUtil where

import Test.HUnit

assertTest :: String -> (a -> Assertion) -> a -> Test
assertTest name func =
  TestLabel name . TestCase . func

assertEqTest :: (Eq a, Show a) => String -> a -> a -> Test
assertEqTest name expected =
  assertTest name (assertEqual name expected)
