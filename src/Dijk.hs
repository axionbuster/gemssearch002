module Dijk (ForM_, Dijk (..), dijk, recon) where

import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Hashable
import           Data.HashPSQ               (HashPSQ)
import qualified Data.HashPSQ               as Q

-- | Partially applied 'forM_'. This is like using a list, but
-- any representation is possible. It's also like using 'Foldable',
-- but the container can be monomorphic. It can also be an entirely abstract
-- process. Must be finite, but can be empty.
type ForM_ a = forall m b. (Monad m) => (a -> m b) -> m ()

-- | Result of 'dijk'. Feed into 'recon' to reconstruct a path.
newtype Dijk k = Dijk (HashPSQ k Int k)

lookup1 :: (Hashable k, Ord k) => k -> HashPSQ k Int v -> Int
lookup1 k q = case Q.lookup k q of
 Just (p, _) -> p
 Nothing     -> maxBound
{-# INLINE lookup1 #-}

dijk
 :: (Hashable k, Ord k)
 => (k -> k -> Int) -- ^ weight (nonnegative)
 -> (k -> ForM_ k) -- ^ neighbors
 -> (k -> Bool) -- ^ stop? (target)
 -> k -- ^ start
 -> Dijk k -- ^ distance and predecessor arrays
dijk weight neighbors stop start = entry where
 -- the starting vertex has a predecessor of self
 entry = evalState go (Q.singleton start 0 start)
 go = do
  bq0 <- get
  case Q.minView bq0 of
   Just (ux, _, _, _) | stop ux -> Dijk <$> get
   Just (ux, ud, _, bq1) -> do
    put bq1
    neighbors ux $ \vx -> do
     bq2 <- get
     let vd0 = lookup1 vx bq2
     let vd1 = ud + weight ux vx
     when (vd1 < vd0) $ put (Q.insert vx vd1 ux bq2)
    go
   Nothing -> Dijk <$> get
{-# INLINE dijk #-}

recon :: (Hashable k, Ord k) => Dijk k -> k -> ([k], Int)
recon (Dijk q) target = go [] target 0 where
 go l u c
  | Just (p, v) <- Q.lookup u q, v /= u = go (v : l) v $! c + p
  | otherwise = (l, c)
{-# INLINE recon #-}
