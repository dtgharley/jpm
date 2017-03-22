{-
   Simple Product Sales Message Processing Application: Common Types
                                                     
   Copryight David Harley 2017                        
-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleContexts #-}
module SM.Common where

import Control.Monad.State
import qualified Data.Map as M

maxFv = 1000000 :: Int -- maximum financial value

srint = 10 :: Int -- interval between sales reports
arint = 50 :: Int  -- interval betwwen adjustment reports

prnt :: String -> MS ()
prnt = liftIO . putStrLn

prnl :: MS ()
prnl = liftIO $ putStrLn ""

data Op = Add | Subtract | Multiply deriving (Eq, Show) -- Adjustment Operator

-- Stored State types

data SV = SV {sales :: Int, price' :: Int, mprice :: Int} deriving (Eq, Show) -- Sales values

data AV = AV {op' :: Op, at :: Int} deriving (Eq, Show) -- Adjustment values

-- Structure for each product: total sales, total sales value, list of sales, list of adjustments

data Pt = Pt {ts :: Int, ta :: Int, ssm :: [SV], ssa :: [AV]} deriving (Eq, Show)

-- State stores message count and map from product name to product structure

data St = Parse | Pause deriving (Eq, Show) -- Monad operational state

data MState = MState {st :: St, cnt :: Int, pmp :: M.Map String Pt} deriving (Eq, Show)

-- stack on IO to allow reports from within

type MS = StateT MState IO

