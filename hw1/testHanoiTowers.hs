{-# OPTIONS_GHC -Wall #-}

import Test.HUnit
import HanoiTowers

hanoiTest1, hanoiTest2 :: Test
hanoiTest1 = TestCase $
    [("a","c"), ("a","b"), ("c","b")] @=? (hanoi 2 "a" "b" "c")
hanoiTest2 = TestCase $
    [("a","c"), ("a","b"), ("c","b"), ("a","c"), ("b","a"), ("b","c"), ("a","c")] @=?
    (hanoi 3 "a" "c" "b")

tests :: Test
tests = TestList [ TestLabel "2-disc case" hanoiTest1
                 , TestLabel "3-disc case" hanoiTest2 ]

main :: IO ()
main = runTestTT tests >> return ()

