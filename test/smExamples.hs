{-
   Simple Product Sales Message Processing Application: smExamples
                                                     
   Copryight David Harley 2017                        
-}
module Main where

import SM.Run

main :: IO ()
main = do
  let s0 = ["apples at 102p", "apple @ 30", "90 apples, 20p", "add 20p apple", "add 2p apples", "Apple, 12p", "cheese @ 30", "90 cheeses, 20p", "add 9p cheese", "add 2p cheeses", "*,10p,banana"]
  let s1 = ["abc at 102p", "def @ 30", "90 abcs, 20p", "add 20p abc", "add 2p def", "apple, 12p", "banana @ 30", "90 apple, 20p", "add 9p abc", "add 2p banana", "*,10p,banana"]
  let s2 = ["abc, at 102p", "def 30", "90 abc, @ 20p", "add 20p abc", "add 2p def", "apple, 12p", "banana 30", "90 apple, 20p", "add 9p abc", "add 2p banana", "*,10p,banana"]
  let s3 = ["abc, 102p", "def 30", "90 abc, 20p", "add 20p abc", "add 2p def", "apple, 12p", "banana 30", "90 apple, 20p", "add 9p abc", "add 2p banana", "*,10p,banana"]
  mm <- runList $ s0 ++ s1 ++ s2 ++ s3 ++ s0 ++ s2 ++ s1 
  putStrLn $ show mm
