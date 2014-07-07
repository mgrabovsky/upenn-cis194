{-# OPTIONS_GHC -Wall #-}

import Control.Monad (void)
import Test.HUnit
import LogAnalysis

parserTest1, parserTest2, parserTest3 :: Test
parserTest1 = TestCase $ assertEqual "in error entry parsing"
    (LogMessage (Error 2) 562 "help help") (parseMessage "E 2 562 help help")
parserTest2 = TestCase $ assertEqual "in informational entry parsing"
    (LogMessage Info 29 "la la la") (parseMessage "I 29 la la la")
parserTest3 = TestCase $ assertEqual "in invalid entry format parsing"
    (Unknown "This is not in the rigth format")
    (parseMessage  "This is not in the rigth format")

tests :: Test
tests = TestList [ TestLabel "message parsing" $
                    TestList [ parserTest1
                             , parserTest2
                             , parserTest3 ] ]

main :: IO ()
main = void $ runTestTT tests

