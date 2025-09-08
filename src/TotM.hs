{-# LANGUAGE PatternSynonyms #-}

module TotM where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import           Control.Monad.ST.Strict
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.STRef.Strict
import           Data.Word

type Cell = Word8 -- only low 2 bits are valid
type TotM = UArray (Int, Int) Cell -- wasteful, but convenient
type TotS s = STUArray s (Int, Int) Cell

pattern Air, Bat, Gem, Obs :: Cell
pattern Air = 0b00
pattern Bat = 0b01
pattern Gem = 0b10
pattern Obs = 0b11
{-# COMPLETE Air, Bat, Gem, Obs #-}

isAir :: Cell -> Bool
isAir = (Air ==)

isSolid :: Cell -> Bool
isSolid = (Air /=)

data Outcome = Running | Won | Lost deriving (Eq, Show)

data ChainState s = ChainState
 (Int, Int)    -- ^ location
 (STRef s Int) -- ^ gem count

-- | Move a piece forward.
chain
 :: ((Int, Int) -> (Int, Int))
 -> (Int, Int)
 -> (Int, Int)
 -> TotS s -> ST s ()
chain nextCoord target ij0 game = entry where
 -- dfs-style algorithm: if movable, keep moving forward
 -- until there is an event. process the event, then continue as necessary.
 -- events:
 -- 1. a gem encounters the target: decrement the gem count.
 --    if positive, continue; otherwise, throw Won.
 -- 2. a bat encounters the target: throw Lost.
 -- 3. counters a movable object: process that and then come back, try again.
 -- 4. counters the wall: stop.
 -- 5. otherwise: move forward; loop.
 entry = do
  gameBounds <- getBounds game
  gemCount <- newSTRef (error "count gems first")
  void $ evalRWST (runExceptT go) gameBounds (ChainState ij0 gemCount)
 bounded ij = inRange ij <$> ask
 swapForward = undefined
 go = undefined
