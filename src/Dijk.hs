module Dijk (ForM_, Dijk (..), dijk, recon) where
import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Hashable
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as H
import           Data.HashPSQ               (HashPSQ)
import qualified Data.HashPSQ               as Q

-- | Partially applied 'forM_'. This is like using a list, but
-- any representation is possible. It's also like using 'Foldable',
-- but the container can be monomorphic. It can also be an entirely abstract
-- process. Must be finite, but can be empty.
type ForM_ a = forall m b. (Monad m) => (a -> m b) -> m ()

-- | Result of 'dijk'
data Dijk k = Dijk
 { _dijk'dist :: HashPSQ k Int () -- ^ Distance to get to each node
 , _dijk'prev :: HashMap k k      -- ^ Predecessor to each node
 }

lookup1 :: (Hashable k, Ord k) => k -> HashPSQ k Int () -> Int
lookup1 k = maybe maxBound fst . Q.lookup k
{-# INLINE lookup1 #-}

dijk
 :: (Hashable k, Ord k)
 => (k -> k -> Int) -- ^ weight (nonnegative)
 -> (k -> ForM_ k) -- ^ neighbors
 -> (k -> Bool) -- ^ stop? (target)
 -> k -- ^ start
 -> Dijk k -- ^ distance and predecessor arrays
dijk weight neighbors stop start = entry where
 entry = evalState go (Q.singleton start 0 (), H.empty)
 go = do
  (bq0, prev0) <- get
  case Q.minView bq0 of
   Just (ux, _, _, _) | stop ux -> uncurry Dijk <$!> get
   Just (ux, ud, _, bq1) -> do
    put (bq1, prev0)
    neighbors ux $ \vx -> do
     (bq2, prev2) <- get
     let vd0 = lookup1 vx bq2
     let vd1 = ud + weight ux vx
     when (vd1 < vd0) $ put (Q.insert vx vd1 () bq2, H.insert vx ux prev2)
    go
   Nothing -> uncurry Dijk <$!> get
{-# INLINE dijk #-}

recon :: (Hashable k, Ord k) => Dijk k -> k -> ([k], Int)
recon ~(Dijk dist prev) target = go [] target where
 go l u
  | Just v <- H.lookup u prev = go (v : l) v
  | otherwise = (l, lookup1 target dist)
{-# INLINE recon #-}
