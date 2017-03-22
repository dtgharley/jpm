{-
   Simple Product Sales Message Processing Application: smLoop
                                                     
   Copryight David Harley 2017                        
-}
module Main where

import SM.Run

main :: IO ()
main = do
  mm <- runLoop
  putStrLn $ show mm
