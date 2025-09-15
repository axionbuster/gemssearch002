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
   isVertical DirUp   = True
   isVertical DirDown = True
   isVertical _       = False
   trav :: (Either (Exc Game) Game, Maybe Direction) -> ForM_ (Direction, (Either (Exc Game) Game, Maybe Direction))
   trav (Right game, lastDir) = \f -> do
    let
     action d = f (d, (moveGame game d, Just d))
     -- after a vertical move, only horizontal moves are allowed and vice-versa.
     -- if there's no previous move, all directions are possible.
     possibleMoves = case lastDir of
      Nothing -> [DirRight, DirLeft, DirUp, DirDown]
      Just ld -> if isVertical ld
       then [DirRight, DirLeft]
       else [DirUp, DirDown]
    forM_ possibleMoves action
   trav _ = const $ pure ()
   done (Left Won {}, _) = True
   done _                = False
  case dijk (\_ _ -> 1) trav done (Right game0, Nothing) of
   outcome -> case recon outcome of
    ([], _, _) -> Nothing
    (states, steps, _) -> do
     let
      toGame (Right g, _)      = g
      toGame (Left (Won g), _) = g
      toGame (Left Lost, _)    = error "solve: losing game in winningstrategy"
     Just (toGame <$> states, steps)
