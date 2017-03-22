{-
   Simple Product Sales Message Processing Application: Parser
                                                     
   Copryight David Harley 2017                        
-}
{-# LANGUAGE OverloadedStrings #-}
module SM.Parser where

import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Text.Regex.PCRE.Light
import SM.Common
import Data.Char

-- Processed message types

data Msg = SngMsg {name :: String, price :: Int} |

           MltMsg {name :: String, price :: Int, quant :: Int} |

           AdjMsg {name :: String, op :: Op, amnt :: Int} deriving (Eq, Show)

{- Very simple 'parser' using regex. N.B pcre-light supports utf8 but requires pcre built with utf8 option.
   That is often not available so not used here, hence supplied message Strings must convert without loss to ByteStrings.
   Best stick to ascii for product names.
   Basic Format of messages is 24 character names, 5 decimal numeric values and comma optional spaces in between.
-}
parseIncoming :: String -> Maybe Msg
parseIncoming sm =
  let smExp = pnm +- oac ++ dvl ++ opc
   in msum [
        mkSM ~= smExp,
        mkMM ~= dvl +- smExp,
        mkAM ~= aop +- dvl ++ opc +- pnm
      ]
  where infixr 5 +-
        (+-) f s = f ++ ocs ++ s
        only mr = compile (B.pack $ "^" ++ ss ++ mr ++ ss ++ "$") []
        infix 4 ~=
        (~=) mf mr = mf $ match (only mr) (B.pack sm) []
        pnm = "([a-z,A-Z]\\w{1,23})"
        oac = "((at|@)" ++ s18 ++ ")?"
        dvl = "(\\d{1,5})"
        ss = "\\s{0,8}"
        s04 = "\\s{0,4}"
        s18 = "\\s{1,8}"
        ocs = "(" ++ s04 ++ "," ++ s04 ++ "|" ++ s18 ++ ")"
        opc = "p?"
        aop = "(\\-|\\+|\\*|add|subtract|sub|multiply|mult|Add|Subtract|Sub|Multiply|Mult)"
        {- Messages are packaged by mk?? for passing to the handler. Any that fail this phase are ignored.
           Use simple pattern matching on strings to check parsing and remove spaces and @s.
           All names are converted to singular form if they are lower case.
        -}
        mkSM (Just (_:t2:_:t3:[])) = Just $ SngMsg (bc t2) (rb t3)
        mkSM (Just (_:t2:_:_:_:t3:[])) = Just $ SngMsg (bc t2) (rb t3)
        mkSM _ = mzero
        mkMM (Just (_:t2:_:t3:_:t4:[])) = Just $ MltMsg (bc t3) (rb t4) (rb t2)
        mkMM (Just (_:t2:_:t3:_:_:_:t4:[])) = Just $ MltMsg (bc t3) (rb t4) (rb t2)
        mkMM _ = mzero
        mkAM (Just (_:t2:_:t4:_:t7:[])) =
          let or = M.lookup (map (toLower) (B.unpack t2)) opMap
           in case or of
               Just go -> Just $ AdjMsg (bc t7) go (rb t4)
               _ -> mzero
        mkAM _ = mzero
        bc = cpd . bu
        rb = read . B.unpack
        opMap = M.fromList [("+", Add), ("add", Add), ("-", Subtract), ("subtract", Subtract), ("sub", Subtract), ("*", Multiply), ("multiply", Multiply), ("mult", Multiply)]
        cpd pn =
          if (pn /= (map toLower pn))
            then
              pn
            else
              let df = M.lookup pn plMap
               in case df of
                 Just sg -> sg
                 Nothing -> reverse $ rmvPs $ reverse pn
        plMap = M.fromList [("sheep", "sheep"), ("molasses", "molasses"), ("mice", "mouse"), ("knives", "knife"), ("axes", "axe")]
        rmvPs ('s':'e':'s':'e':'e':rest) = 'e':'s':'e':'e':rest
        rmvPs ('s':'e':'s':'u':'o':rest) = 'e':'s':'u':'o':rest
        rmvPs ('s':'e':'s':'a':rest) = 'e':'s':'a':rest
        rmvPs ('s':'e':'h':'s':rest) = 'h':'s':rest
        rmvPs ('s':'e':'h':'c':rest) = 'h':'c':rest
        rmvPs ('s':'e':'c':rest) = 'e':'c':rest
        rmvPs ('s':'e':'x':rest) = 'x':rest
        rmvPs ('s':'e':'s':rest) = 's':rest
        rmvPs ('s':rest) = rest
        rmvPs pl = pl
        bu = B.unpack

