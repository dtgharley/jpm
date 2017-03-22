{-
   Simple Product Sales Message Processing Application: Tests 
                                                     
   Copryight David Harley 2017                        
-}

module Main where

import Test.HUnit
import SM.Common
import SM.Parser
import SM.Handler
import Control.Monad.State
import qualified Data.Map as M

parseTest_01 :: Test
parseTest_01 = 
    TestCase $ assertEqual "Parse single sale"
                           (Just $ SngMsg "apple" 2) (parseIncoming "apple 2")

parseTest_02 :: Test
parseTest_02 = 
    TestCase $ assertEqual "Parse single sale"
                           (Just $ SngMsg "apple" 2) (parseIncoming "apples at 2p")

parseTest_03 :: Test
parseTest_03 = 
    TestCase $ assertEqual "Parse multiple sale"
                           (Just $ MltMsg "Apple" 50 20) (parseIncoming "20 Apple @ 50")

parseTest_04 :: Test
parseTest_04 = 
    TestCase $ assertEqual "Parse multiple sale"
                           (Just $ MltMsg "axe" 10 2) (parseIncoming "2 axes  10")

parseTest_05 :: Test
parseTest_05 = 
    TestCase $ assertEqual "Parse adjustment instruction"
                           (Just $ AdjMsg "axe" Add 10) (parseIncoming "Add 10p axes")

parseTest_06 :: Test
parseTest_06 = 
    TestCase $ assertEqual "Parse adjustment instruction"
                           (Just $ AdjMsg "cheese" Multiply 3) (parseIncoming "* 3 cheeses")

parseTest_07 :: Test
parseTest_07 = 
    TestCase $ assertEqual "Parse adjustment instruction"
                           (Just $ AdjMsg "house" Subtract 2) (parseIncoming "- 2 houses")

parseTest_08 :: Test
parseTest_08 = 
    TestCase $ assertEqual "Parse adjustment instruction"
                           (Just $ AdjMsg "house" Subtract 2) (parseIncoming "- 2 house")

handlerTest_01 :: Test
handlerTest_01 =
    TestCase $ assertEqual "handler test" (MState Parse 1 (M.fromList [("peach", Pt 20 1000 [SV 20 50 50] [])]))
                                          =<< (execStateT (mHandler $ "20 peaches at 50p") (MState Parse 0 M.empty))

main :: IO Counts
main = runTestTT $ TestList [parseTest_01, parseTest_02, parseTest_03, parseTest_04, parseTest_05, parseTest_06, parseTest_07, parseTest_08, handlerTest_01]
