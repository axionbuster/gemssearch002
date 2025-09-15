-- |
-- Module      : SolveTotM2
-- Description : High-level solver for the Gem Seeker minigame
-- Copyright   : (c) 2025 axionbuster
-- License     : BSD-3-Clause
-- Maintainer  : axionbuster
--
-- This module provides a high-level interface for solving the Gem Seeker
-- minigame. It uses Dijkstra's algorithm, implemented in the 'Dijk' module, to
-- find the shortest path from an initial game state to a winning state. The
-- solver explores possible moves, transitioning between game states until a
-- solution is found or all possibilities are exhausted.
--
-- The main function, 'solve', takes an initial board layout and a target
-- location, and returns a list of game states and the corresponding sequence of
-- moves that lead to a win. If no solution is possible, it returns 'Nothing'.
module SolveTotM2 (solve) where

import           Control.Monad
import           Data.Array.Unboxed
import           Dijk
import           TotM2

-- | Solve the game
solve :: UArray (Int, Int) Cell -> (Int, Int) -> Maybe ([Game], [Direction])
solve board target = do
 let
  (_, (h, w)) = bounds board
  game0 = buildGame (h + 1) (w + 1) target (board !)
 if _game'gemCnt game0 == 0 then error "solve: gems == 0" else do
  let
   trav :: Either (Exc Game) Game -> ForM_ (Direction, Either (Exc Game) Game)
   trav (Right game) = \f -> do
    let action d = f (d, moveGame game d)
    -- order doesn't matter, since we use the same (pure) game state (game).
    void $ action DirRight
    void $ action DirLeft
    void $ action DirUp
    void $ action DirDown
   trav _ = const $ pure ()
   done (Left Won {}) = True
   done  _            = False
  case dijk (\_ _ -> 1) trav done (Right game0) of
   outcome -> case recon outcome of
    ([], _, _) -> Nothing
    (states, steps, _) -> do
     let toGame (Right g)      = g
         toGame (Left (Won g)) = g
         toGame (Left Lost)    = error "solve: losing game in winning strategy"
     Just (toGame <$> states, steps)
