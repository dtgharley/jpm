{-
   Simple Product Sales Message Processing Application: Handler Tests 
                                                     
   Copryight David Harley 2017                        
-}

module Main where

import Test.HUnit
import SM.Common
import SM.Handler
import Control.Monad.State
import qualified Data.Map as M

the msg map = 
    TestCase $ assertEqual "handler test" (MState Parse 1 (M.fromList map))
                                          =<< (execStateT (mHandler $ msg) (MState Parse 0 M.empty))
    
handlerTest_01 :: Test
handlerTest_01 =
    the "20 peaches at 50p" [("peach", Pt 20 1000 [SV 20 50 50] [])]

handlerTest_02 :: Test
handlerTest_02 =
    the "* 2 peaches" [("peach", Pt 0 0 [] [AV Multiply 2])]

main :: IO Counts
main = runTestTT $ TestList [handlerTest_01, handlerTest_02]

