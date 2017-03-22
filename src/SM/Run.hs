{-
   Simple Product Sales Message Processing Application: Run
                                                     
   Copyright David Harley 2017                        
-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleContexts #-}
module SM.Run (runList, runLoop, MState) where

import Control.Monad.State
import qualified Data.Map as M
import Data.Char
import System.IO
import SM.Common
import SM.Parser
import SM.Handler
import Data.Char

prCT = prnt "Press C to continue or any other key to terminate."

chQt = (flip elem ["q", "quit"]) . (map toLower)

runList :: [String] -> IO MState
runList ml = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  execStateT (lh ml) (MState Parse 0 M.empty)
  where
    lh (m:ms) = do
      cms <- get
      case (st cms) of
        Parse -> do
          if (chQt m)
            then
              return ()
            else do
              mHandler m
              lh ms
        Pause -> do
          prCT
          c <- lGetChar
          if (toLower c == 'c')
            then do
              put $ cms {st = Parse}
              lh (m:ms)
            else
              return ()
    lh _ = return ()  
    lGetChar = liftIO $ hGetChar stdin

runLoop :: IO MState
runLoop = do
  let prompt = "msg:"
  hSetBuffering stdout NoBuffering
  execStateT (ph prompt) (MState Parse 0 M.empty)
  where
    ph p = do
      cms <- get
      case (st cms) of
        Parse -> do
          s <- lGetLine p
          if (((flip elem ["q", "quit"]) . (map toLower)) s)
            then
              return ()
            else do
              mHandler s
              ph p
        Pause -> do
          prCT
          c <- lGetChar
          if (toLower c == 'c')
            then do
              put $ cms {st = Parse}
              lRstSi
              ph p
            else
              return ()
    lGetLine p = liftIO $ do putStr p; getLine
    lGetChar = liftIO $ do hSetEcho stdin False; hSetBuffering stdin NoBuffering; hGetChar stdin
    lRstSi = liftIO $ do hSetEcho stdin True; hSetBuffering stdin LineBuffering

