{-# OPTIONS_GHC -Wall #-}

import Control.Monad (void)
import Test.HUnit
import CreditCard

digitsTest1, digitsTest2, digitsTest3, digitsTest4 :: Test
digitsTest1 = TestCase $ assertEqual "in digits decomposition"
    [1,2,3,4] (toDigits 1234)
digitsTest2 = TestCase $ assertEqual "in reverse digits decomposition"
    [4,3,2,1] (toDigitsRev 1234)
digitsTest3 = TestCase $ assertEqual "in zero decomposition"
    [] (toDigits 0)
digitsTest4 = TestCase $ assertEqual "in negative number decomposition"
    [] (toDigits (-2014))

doublingTest1, doublingTest2 :: Test
doublingTest1 = TestCase $ assertEqual "in every-other doubling of even-length list"
    [16,7,12,5] (doubleEveryOther [8,7,6,5])
doublingTest2 = TestCase $ assertEqual "in every-other doubling of odd-length list"
    [1,4,3] (doubleEveryOther [1,2,3])

sumTest :: Test
sumTest = TestCase $ assertEqual "in sum of digits"
    22 (sumDigits [16,7,12,5])

validationTest1, validationTest2 :: Test
validationTest1 = TestCase $ assertBool "in validation of valid input"
    (validate 4012888888881881)
validationTest2 = TestCase $ assertBool "in validation of invalid input"
    (not (validate 4012888888881882))

tests :: Test
tests = TestList [ TestLabel "number to digits decomposition" $
                    TestList [ digitsTest1
                             , digitsTest2
                             , digitsTest3
                             , digitsTest4 ]
                 , TestLabel "doubling routine" $
                    TestList [ doublingTest1
                             , doublingTest2 ]
                 , TestLabel "sum of digits" $ sumTest
                 , TestLabel "number validation" $
                    TestList [ validationTest1
                             , validationTest2 ] ]

main :: IO ()
main = void $ runTestTT tests

