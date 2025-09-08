module Dijk (ForM_, Dijk, _dijk'target, dijk, recon) where

import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Hashable
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as M
import qualified Data.HashPSQ               as Q

-- node information:
--
-- either start node (cost only) or step node (cost + predecessor + how)
--
-- ordering the more common (almost always `Step`) constructor first. known to
-- improve performance across the board to order constructors by frequency.
data Node k h = Step Int k h | Start Int
 deriving (Show)

-- | Partially applied 'forM_'. This is like using a list, but
-- any representation is possible. It's also like using 'Foldable',
-- but the container can be monomorphic. It can also be an entirely abstract
-- process. Must be finite, but can be empty.
type ForM_ a = forall m b. (Monad m) => (a -> m b) -> m ()

-- | Result of 'dijk'. Feed into 'recon' to reconstruct a path.
data Dijk k h = Dijk (HashMap k (Node k h)) (Maybe k)
 deriving
  ( Show -- ^ Debugging accommodation
  )

-- | The target vertex detected (if any).
_dijk'target :: Dijk k h -> Maybe k
_dijk'target ~(Dijk _ t) = t

lookupCost :: (Hashable k) => k -> HashMap k (Node k h) -> Int
lookupCost k m = case M.lookup k m of
 Just (Start cost)    -> cost
 Just (Step cost _ _) -> cost
 Nothing              -> maxBound
{-# INLINE lookupCost #-}

dijk
 :: (Hashable k, Ord k)
 => (k -> k -> Int) -- ^ weight (nonnegative)
 -> (k -> ForM_ (h, k)) -- ^ neighbors: yields (how, nextState) pairs
 -> (k -> Bool) -- ^ stop? (target)
 -> k -- ^ start
 -> Dijk k h -- ^ distance and predecessor arrays
dijk weight neighbors stop start = entry where
 -- the starting vertex has no predecessor
 entry = evalState go (Q.singleton start 0 (), M.singleton start (Start 0))
 go = do
  (bq0, costs) <- get
  case Q.minView bq0 of
   Just (ux, _, _, _) | stop ux -> Dijk <$> gets snd <*> pure (Just ux)
   Just (ux, ud, _, bq1) -> do
    put (bq1, costs)
    neighbors ux $ \(howToVx, vx) -> do
     (bq2, costs2) <- get
     let vd0 = lookupCost vx costs2
     let vd1 = ud + weight ux vx
     when (vd1 < vd0) $ do
      let newQueue = Q.insert vx vd1 () bq2
      let newCosts = M.insert vx (Step vd1 ux howToVx) costs2
      put (newQueue, newCosts)
    go
   Nothing -> Dijk <$> gets snd <*> pure Nothing
{-# INLINE dijk #-}

recon :: (Hashable k) => Dijk k h -> k -> ([k], [h], Int)
recon (Dijk costs _) target = go [] [] target 0 where
 go stateList moveList u c
  | Just (Step d v h) <- M.lookup u costs = go (v : stateList) (h : moveList) v $! c + d
  | Just (Start d) <- M.lookup u costs = (stateList, moveList, c + d)
  | otherwise = (stateList, moveList, c)
{-# INLINE recon #-}
