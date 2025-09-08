module Dijk (ForM_, Dijk, dijk, recon) where

import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Hashable
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as M
import qualified Data.HashPSQ               as Q

-- | Strict pair for storing cost and predecessor
data Pair a b = Pair !a !b
 deriving (Show, Eq)

-- | Partially applied 'forM_'. This is like using a list, but
-- any representation is possible. It's also like using 'Foldable',
-- but the container can be monomorphic. It can also be an entirely abstract
-- process. Must be finite, but can be empty.
type ForM_ a = forall m b. (Monad m) => (a -> m b) -> m ()

-- | Result of 'dijk'. Feed into 'recon' to reconstruct a path.
newtype Dijk k = Dijk (HashMap k (Pair Int k))
 deriving
  ( Show -- ^ Debugging accommodation
  )

lookupCost :: (Hashable k) => k -> HashMap k (Pair Int k) -> Int
lookupCost k m = case M.lookup k m of
 Just (Pair cost _) -> cost
 Nothing            -> maxBound
{-# INLINE lookupCost #-}

dijk
 :: (Hashable k, Ord k)
 => (k -> k -> Int) -- ^ weight (nonnegative)
 -> (k -> ForM_ k) -- ^ neighbors
 -> (k -> Bool) -- ^ stop? (target)
 -> k -- ^ start
 -> Dijk k -- ^ distance and predecessor arrays
dijk weight neighbors stop start = entry where
 -- the starting vertex has a predecessor of self
 entry = evalState go (Q.singleton start 0 (), M.singleton start (Pair 0 start))
 go = do
  (bq0, costs) <- get
  case Q.minView bq0 of
   Just (ux, _, _, _) | stop ux -> Dijk <$> gets snd
   Just (ux, ud, _, bq1) -> do
    put (bq1, costs)
    neighbors ux $ \vx -> do
     (bq2, costs2) <- get
     let vd0 = lookupCost vx costs2
     let vd1 = ud + weight ux vx
     when (vd1 < vd0) $ do
      let newQueue = Q.insert vx vd1 () bq2
      let newCosts = M.insert vx (Pair vd1 ux) costs2
      put (newQueue, newCosts)
    go
   Nothing -> Dijk <$> gets snd
{-# INLINE dijk #-}

recon :: (Hashable k) => Dijk k -> k -> ([k], Int)
recon (Dijk costs) target = go [] target 0 where
 go l u c
  | Just (Pair d v) <- M.lookup u costs, v /= u = go (v : l) v $! c + d
  | otherwise = (l, c)
{-# INLINE recon #-}
