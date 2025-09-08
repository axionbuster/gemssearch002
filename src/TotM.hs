{-# LANGUAGE PatternSynonyms #-}

{- |
Module      : TotM
Description : Tomb of the Mask+ Gem Seeker minigame solver
Copyright   : (c) 2025 axionbuster  
License     : BSD-3-Clause
Maintainer  : axionbuster

Solves the Gem Seeker minigame from Tomb of the Mask+ using efficient
pathfinding algorithms.
-}
module TotM
 ( GameState (..), TotM, Direction (..), Cell, Outcome (..)
 , pattern Air, pattern Bat, pattern Gem, pattern Obs
 , solve, createBoard, showBoard, applyGravity
 , unTotM, totMBounds, totMIndex, checkOutcome
 ) where

import           Control.Monad
import           Control.Monad.ST.Strict
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.Array.Unsafe
import           Data.Hashable
import           Data.Word
import           Dijk
import           Prelude                 hiding (Left, Right)

type Cell = Word8 -- only low 2 bits are valid

newtype TotM = TotM (UArray (Int, Int) Cell)
 deriving (Eq, Show, Ord)

type TotS s = STUArray s (Int, Int) Cell

pattern Air, Bat, Gem, Obs :: Cell
pattern Air = 0b00
pattern Bat = 0b01
pattern Gem = 0b10
pattern Obs = 0b11
{-# COMPLETE Air, Bat, Gem, Obs #-}

isAir :: Cell -> Bool
isAir = (Air ==)

data Outcome = Running | Won | Lost deriving (Eq, Show)

data Direction = Up | Down | Left | Right deriving (Eq, Show, Enum, Bounded)

instance Hashable Direction where
 hashWithSalt s = hashWithSalt s . fromEnum

instance Hashable TotM where
 hashWithSalt s (TotM arr) = hashWithSalt s (elems arr)

-- Helper functions for TotM newtype
mkTotM :: UArray (Int, Int) Cell -> TotM
mkTotM = TotM

unTotM :: TotM -> UArray (Int, Int) Cell
unTotM (TotM arr) = arr

totMBounds :: TotM -> ((Int, Int), (Int, Int))
totMBounds (TotM arr) = bounds arr

totMIndex :: TotM -> (Int, Int) -> Cell
totMIndex (TotM arr) = (arr !)

directionVector :: Direction -> (Int, Int)
directionVector Up    = (-1, 0)
directionVector Down  = (1, 0)
directionVector Left  = (0, -1)
directionVector Right = (0, 1)

nextCoord :: Direction -> (Int, Int) -> (Int, Int)
nextCoord dir (r, c) = let (dr, dc) = directionVector dir in (r + dr, c + dc)

-- The Gem Seeker game
--
-- Purpose: collect all "gems" without hitting a bat.
-- Setup: a grid of blocks, four kinds of which exist:
--  00 - air
--  01 - bat - solid, movable
--  10 - gem - solid, movable
--  11 - obs(tacle)/wall - solid
-- Additionally, there is exactly one "target" cell, which is fixed
-- and must ALWAYS be an Air cell. No gem, bat, or obstacle should ever
-- occupy the target position in any valid game state.
-- Physics (Basic):
-- - Kind of like Mercury Meltdown Ultimate, but discrete and simpler.
-- - Each move consists of changing the *gravity* to point in one of four
--   directions.
-- - Then all movable pieces move toward the direction of the gravity, but
--   solid pieces cannot teleport on top of each other.
-- Physics (Tricky):
-- - When a gem slides into the "target," it disappears *immediately*.
--   This means no gem should ever be found at the target position.
-- - If a bat will hit the player right after the last gem hits the target cell
--   all in the same move then this is still considered a win.
-- - Sometimes the state does not change when gravity changes. That's OK if
 --  physically necessitated.

-- | Move a piece forward.
chain
 :: ((Int, Int) -> (Int, Int))
 -> (Int, Int)
 -> (Int, Int)
 -> TotS s -> ST s ()
chain nextCoordFn target ij0 game = go ij0 where
 -- Simple iterative algorithm: keep moving forward until stopped
 go ij = do
  gameBounds <- getBounds game
  let inBounds = inRange gameBounds
  -- Only proceed if we're in bounds and have something to move
  when (inBounds ij) $ do
   cell <- readArray game ij
   unless (isAir cell) $ do
    let ij' = nextCoordFn ij
    if not (inBounds ij')
    then return () -- hit boundary, stop
    else do
     cell' <- readArray game ij'
     case cell' of
      Air -> do
       -- Move forward
       writeArray game ij Air
       if ij' == target && cell == Gem
       then writeArray game ij' Air  -- gem disappears when hitting target
       else writeArray game ij' cell
       go ij'
      Obs -> return () -- hit obstacle, stop
      _ -> do -- hit movable object
       chain nextCoordFn target ij' game -- process the object we hit first
       -- Try again - the space might be clear now
       cell'' <- readArray game ij'
       when (cell'' == Air) $ do
        writeArray game ij Air
        if ij' == target && cell == Gem
        then writeArray game ij' Air  -- gem disappears when hitting target
        else writeArray game ij' cell
        go ij'

countGems :: TotS s -> ST s Int
countGems game = do
 gameBounds <- getBounds game
 let coords = range gameBounds
 cells <- mapM (readArray game) coords
 pure $ length $ filter (== Gem) cells

{- | 
Apply gravity in a direction to the entire game.

Moves all movable objects (gems and bats) in the specified direction until
they hit obstacles, boundaries, or other objects. Returns the new board
state and the game outcome.
-}
applyGravity :: Direction -> (Int, Int) -> TotM -> ST s (TotM, Outcome)
applyGravity dir target game = do
 mutableGame <- thaw (unTotM game)
 gameBounds <- getBounds mutableGame
 let coords = range gameBounds

 -- Move all movable objects
 forM_ coords $ \ij -> do
  cell <- readArray mutableGame ij
  when (cell == Bat || cell == Gem) $
   chain (nextCoord dir) target ij mutableGame

 -- Check outcome
 outcome <- checkOutcome target mutableGame
 finalGame <- unsafeFreeze mutableGame
 pure (mkTotM finalGame, outcome)

checkOutcome :: (Int, Int) -> TotS s -> ST s Outcome
checkOutcome target game = do
 targetCell <- readArray game target
 gemCount <- countGems game
 pure $ case targetCell of
  Bat -> Lost
  _ | gemCount == 0 -> Won    -- all gems collected wins!
  _ -> Running

-- | Game state for pathfinding
data GameState = GameState
 { gsBoard  :: TotM
 , gsTarget :: (Int, Int)
 } deriving (Eq, Show, Ord)

instance Hashable GameState where
 hashWithSalt s (GameState board target) =
  s `hashWithSalt` board `hashWithSalt` target

-- | Get all possible next states from current state
neighbors :: GameState -> ForM_ (Direction, GameState)
neighbors (GameState board target) action =
 let directions = [Up, Down, Left, Right] in
 forM_ directions $ \dir -> do
  let (newBoard, outcome) = runST $ applyGravity dir target board
  when (outcome /= Lost) $
   void $ action (dir, GameState newBoard target)

{- | 
Solve the gem seeker puzzle using uniform-cost search.

Finds the optimal sequence of gravity changes to collect all gems and reach
the target, or returns Nothing if no solution exists.
-}
solve :: GameState -> Maybe [Direction]
solve startState =
 let
  isWon (GameState board target) =
   let
    outcome = runST $ do
     mutableGame <- thaw (unTotM board)
     checkOutcome target mutableGame
   in outcome == Won

  weight _ _ = 1  -- each move costs 1

  dijkResult = dijk weight neighbors isWon startState
  maybeWinState = _dijk'target dijkResult
 in
  case maybeWinState of
   Nothing -> Nothing  -- No winning state found
   Just winState -> 
    let (_, moveList, _) = recon dijkResult winState
    in if null moveList
       then Nothing
       else Just moveList


{- | 
Create a game board from a 2D list of cells.

Takes a list of rows (each row is a list of cells) and a target position,
returns the game board and target position.
-}
createBoard :: [[Cell]] -> (Int, Int) -> (TotM, (Int, Int))
createBoard cells target =
 let
  rows = length cells
  cols = case cells of
   []  -> 0
   r:_ -> length r
  gameBounds = ((0, 0), (rows - 1, cols - 1))
  gameAssocs =
   [ ((r, c), cell)
   | (r, row) <- zip [0..] cells, (c, cell) <- zip [0..] row
   ]
  board = array gameBounds gameAssocs
 in (mkTotM board, target)

{- | 
Pretty print a game board for debugging.

Displays the board with 'T' for target, 'G' for gems, 'B' for bats, 
'#' for obstacles, and '.' for air.
-}
showBoard :: TotM -> (Int, Int) -> String
showBoard board target = unlines $ map showRow [r1..r2]
 where
  ((r1, c1), (r2, c2)) = totMBounds board
  showRow r = [showCell (r, c) | c <- [c1..c2]]
  showCell ij
   | ij == target = 'T'
   | otherwise = case totMIndex board ij of
    Air -> '.'
    Bat -> 'B'
    Gem -> 'G'
    Obs -> '#'
