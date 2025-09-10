{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module TotM2 where

import Control.Monad
import Control.Monad.ST.Strict
import Data.Array.Unboxed
import Data.Array.ST
import Data.Bits
import TotM (Cell, pattern Air, pattern Bat, pattern Gem, pattern Obs)

mod4 :: Int -> Int
mod4 = (`mod` 4)

cdiv4 :: Int -> Int
cdiv4 ((`quotRem` 4) -> (q, r)) = q + fromEnum (r /= 0)

quotRem4 :: Int -> (Int, Int)
quotRem4 n = (n .>>. 2, n .&. 0b11)

newtype IA = IA (UArray (Int, Int) Word)
newtype SA s = SA (STUArray s (Int, Int) Word)

w2cell :: Word -> Cell
w2cell = fromIntegral

getIA :: IA -> (Int, Int) -> Cell
getIA (IA ia) (r, c) = case quotRem4 c of
 (cq, cr) -> w2cell $ ((ia ! (r, cq)) .>>. cr) .&. 0b11

(!-) :: IA -> (Int, Int) -> Cell
(!-) = getIA

readSA :: SA s -> (Int, Int) -> ST s Cell
readSA (SA sa) (r, c) = case quotRem4 c of
 (cq, cr) -> (\w -> w2cell $ (w .>>. cr) .&. 0b11) <$!> readArray sa (r, cq)
