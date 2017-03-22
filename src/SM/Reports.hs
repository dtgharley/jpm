{-
   Simple Product Sales Message Processing Application: Reports
                                                     
   Copryight David Harley 2017                        
-}
{-# LANGUAGE RecordWildCards #-}
module SM.Reports (logSalesReport, logAdjReport) where

import qualified Data.Map as M
import Data.Char
import SM.Common

logSalesReport :: M.Map String Pt -> MS () 
logSalesReport nm = do
   prnl
   prnt "Sales Report:"
   prnt $ "Product" +. "Total Sales" +. "Total Value"
   mapM_ prPt (M.toList nm)
   prnl
   where
     prPt (n, Pt{..}) = prnt $ n +.# ts ~.# ta -- print the sales totals for each product
     infixr 5 ~.#
     (~.#) = fmt4s3 show gshow
     gshow s = if (abs s == maxFv)
                 then "suspended"
                 else show s

logAdjReport :: M.Map String Pt -> MS () 
logAdjReport nm = do
   let lm = M.toList nm
   prnl
   prnt "Adjustments Report:"
   if (gotAdj lm)
     then do
       prnt $ "Product" +. "Operation" +. "Amount"
       mapM_ prnAdj lm
     else
       prnt "No adjustments foound."
   prnt ""
   where
     gotAdj l = not . null $ filter id $ map (\x -> not . null $ ssa $ snd x) l
     prnAdj p = prAj (fst p, reverse $ ssa $ snd p)
     prAj (n, (av:avs)) = do
       prAh n av
       mapM_ (prAh (replicate (length n) ' ')) avs
     prAj (_, _) =
       return ()
     prAh n AV{..} = prnt $ n +.# op' ~.# at -- print the product name (for first entry) and operator and amount for each adjustment
     infixr 5 ~.#
     (~.#) = fmt4s3 os show
     os op = case op of
               Add -> "+"
               Subtract -> "-"
               Multiply -> "*"

fmt4s3 f1 f2 f s = (f1 f) ++ (t4s3 $ f1 f) ++ (f2 s)
infixr 5 +.
(+.) f s = f ++ (t3 f) ++ s
infixr 5 +.#
(+.#) f s = f ++ (ts3 f) ++  s
t x = replicate (x * 8) ' '
t3 f = drop (length f) $ t 3
s x = replicate x ' '
s3 = s 3
ts3 f = (t3 f) ++ s3
t4s3 f = (t3 f)

