{-
   Simple Product Sales Message Processing Application: Handler
                                                     
   Copyright David Harley 2017                        
-}
{-# LANGUAGE RecordWildCards #-}
module SM.Handler (MState, mHandler) where

import Control.Monad.State
import qualified Data.Map as M
import SM.Parser
import SM.Common
import SM.Reports

{- Mesaage handler functions update the lists of recorded sales and adjustment instructions for each product and update its running totals. Any values greater than maxFv will
   result in that value being frozen at +/- maxFv. Just prevent overflow for now. Messages will still be accepted for the product but it will be listed as suspended if any of its
   total values have overflowed. N.B we allow negative values.
-}

mHandler :: String -> MS ()
mHandler is = do
  let mm = parseIncoming is
   in case mm of
        Just m -> pmHandler m
        Nothing -> return ()

pmHandler :: Msg -> MS ()
pmHandler SngMsg{..} =
  updateSV name 1 price

pmHandler MltMsg{..} =
  updateSV name quant price

pmHandler AdjMsg{..} =
  updateAV name op amnt

updateSV :: String -> Int -> Int -> MS ()
updateSV nn qt pr =
  updatePt nn $ Left $ SV qt pr pr

updateAV :: String -> Op -> Int -> MS ()
updateAV nn op at =
  updatePt nn $ Right $ AV op at

{- Main product structure update function. Accepts a product name and Either a sales instruction or adjustment instruction.
   Logs reports and pauses the application as required.
-}
updatePt :: String -> Either SV AV ->  MS ()
updatePt nn sa = do
  cms <- get
  let cs = pmp cms
      ce = M.lookup nn cs
      (ts, ta, s, a) = case ce of
                 Nothing -> case sa of
                              Left sv@SV{..} -> (sales, sales * price', [sv], [])
                              Right av -> (0, 0, [], [av])
                 Just Pt{..} -> case sa of
                              Left sv@SV{..} -> (ts + sales, ta + (sales * price'), sv : ssm, ssa)
                              Right av -> (ts, sav av ts ta, mav av ssm, av : ssa)
      nc = (cnt cms) + 1
      nm = M.insert nn (Pt ts ta s a) cs
  if (nc `mod` srint == 0)
    then
      logSalesReport nm
    else return ()
  nt <- if (nc `mod` arint == 0)
         then do
           prnt $ (show nc) ++ " messages processed. Message handling suspended."
           logAdjReport nm
           return Pause
         else
           return $ st cms
  put $ MState nt nc nm

{- Map an adjustment over a list of sales records. We maintain a modified price for each sale instruction as well as the original.
   Just demonstrates we maintain individual sales records as required in a easy to process form.
-}
mav :: AV -> [SV] -> [SV]
mav AV{..} ss =
  map (aop op' at) ss
  where
    aop o a =
      \sv -> SV (sales sv) (price' sv) $
        checkPrice (mprice sv) $ 
          case o of
            Add -> a + (mprice sv)
            Subtract -> (mprice sv) - a
            Multiply -> a * (mprice sv)

-- apply an adjustment to the totals of a product.
sav :: AV -> Int -> Int -> Int
sav AV{..} ts ta =
  checkPrice ta $
    case op' of
      Add -> ta + (ts * at)
      Subtract -> ta - (ts * at)
      Multiply -> ta * at

-- check for over/underflow and fix at that value if detected.
checkPrice :: Int -> Int -> Int
checkPrice op np  =
  if (abs np > maxFv)
    then
      if (np < 0)
        then -maxFv
        else maxFv
    else
      if (abs op == maxFv)
        then op
        else np
