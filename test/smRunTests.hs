{-
   Simple Product Sales Message Processing Application: Run Tests 
                                                     
   Copryight David Harley 2017                        
-}

module Main where

import Test.HUnit
import SM.Common
import SM.Run
import Control.Monad.State
import qualified Data.Map as M

rlt ml tl = do
    TestCase $ assertEqual "run list test" tl
                                           =<< (do rs <- runList ml;
                                                   let pl = M.toList $ pmp rs;
                                                   return $ (cnt rs, map (\x -> (ts $ snd x, ta $ snd x)) pl))
    
runListTest_01 :: Test
runListTest_01 =
    rlt ["20 oranges at 10p", "- 2 orange"] (2, [(20, 160)])

runListTest_02 :: Test
runListTest_02 =
    rlt ["20 oranges at 10p", "- 2 orange", "carrot 1p", "* 100 carrots"] (4, [(1, 100), (20, 160)])

main :: IO Counts
main = runTestTT $ TestList [runListTest_01, runListTest_02]

