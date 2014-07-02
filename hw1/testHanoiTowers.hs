{-# OPTIONS_GHC -Wall #-}

import Test.HUnit
import HanoiTowers

hanoiTest1, hanoiTest2 :: Test
hanoiTest1 = TestCase $
    [("a","c"), ("a","b"), ("c","b")] @=? (hanoi 2 "a" "b" "c")
hanoiTest2 = TestCase $
    [("a","c"), ("a","b"), ("c","b"), ("a","c"), ("b","a"), ("b","c"), ("a","c")] @=?
    (hanoi 3 "a" "c" "b")

hanoi4Test1, hanoi4Test2 :: Test
hanoi4Test1 = TestCase $
    [("A","B"), ("A","C"), ("A","D"), ("C","D"), ("B","D")] @=?
    (hanoi4 3 "A" "D" "B" "C")
hanoi4Test2 = TestCase $ 129 @=? (length $ hanoi4 15 "A" "D" "B" "C")

tests :: Test
tests = TestList [ TestLabel "3-peg towers" $
                    TestList [ TestLabel "2-disc case" hanoiTest1
                             , TestLabel "3-disc case" hanoiTest2 ]
                 , TestLabel "4-peg towers" $
                    TestList [ TestLabel "3-disc case" hanoi4Test1
                             , TestLabel "15-disc case length" hanoi4Test2 ] ]

main :: IO ()
main = runTestTT tests >> return ()

