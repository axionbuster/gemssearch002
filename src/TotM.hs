{-# LANGUAGE PatternSynonyms #-}

{- |
Module      : TotM
Description : Tomb of the Mask+ Gem Seeker minigame solver
Copyright   : (c) 2025 axionbuster
License     : BSD-3-Clause
Maintainer  : axionbuster

Solves the Gem Seeker minigame from Tomb of the Mask+ using efficient
pathfinding algorithms with exception-based game state management.

IMPORTANT: This module uses exceptions internally for control flow.
All public functions return pure values, but internal ST computations
may throw GameException. This is by design and documented behavior.
-}
module TotM
 ( -- * Game Types
   GameState(..)         -- Opaque, only for step-by-step operation and debugging
 , TotM                  -- Opaque board representation
 , Direction(..)
 , Cell
 , GameResult
 , GameException(..)
 , Outcome(..)           -- For backward compatibility

 -- * Cell Patterns
 , pattern Air, pattern Bat, pattern Gem, pattern Obs

 -- * Construction
 , createBoard
 , mkGameState

 -- * Game Operations
 , stepGame             -- Apply one direction, returns result or new state
 , solve                -- High-level solver

 -- * Display
 , showBoard
 , showGameState

 -- * Query Operations
 , getBoardBounds
 , getCell
 ) where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.ST.Strict
import           Data.Array.ST
import qualified Data.Array.ST           as ST
import           Data.Array.Unboxed
import           Data.Array.Unsafe
import           Data.Hashable
import           Data.List               (groupBy)
import           Data.Word
import           Dijk

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

-- | Game exceptions used for control flow
data GameException = BatHitTarget | AllGemsCollected
 deriving (Eq, Show)

instance Exception GameException

-- | Old outcome type for backward compatibility
data Outcome = Running | Won | Lost deriving (Eq, Show)

-- | Result of a game operation - either an exception (end state) or continue
type GameResult = Either GameException ()

data Direction = DirUp | DirDown | DirLeft | DirRight deriving (Eq, Show, Enum, Bounded)

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
directionVector DirUp    = (-1, 0)
directionVector DirDown  = (1, 0)
directionVector DirLeft  = (0, -1)
directionVector DirRight = (0, 1)

nextCoord :: Direction -> (Int, Int) -> (Int, Int)
nextCoord dir (r, c) = let (dr, dc) = directionVector dir in (r + dr, c + dc)

forsI_ :: (Ord t, Monad f, Num t) => t -> t -> (t -> f a) -> f ()
forsI_ s e f = go s where
 go i = when (i <= e) $ f i >> go (i + 1)
{-# INLINE forsI_ #-}

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
    then pure () -- hit boundary, stop
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
      Obs -> pure () -- hit obstacle, stop
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

{- OLD API - TO BE REMOVED
Apply gravity in a direction to the entire game.

Moves all movable objects (gems and bats) in the specified direction until
they hit obstacles, boundaries, or other objects. Returns the new board
state and the game outcome.
-}
applyGravity :: Direction -> (Int, Int) -> TotM -> ST s (TotM, Outcome)
applyGravity dir target game = do
 mutableGame <- thaw (unTotM game)
 ((_, _), (hm1, wm1)) <- ST.getBounds mutableGame

 -- Move all movable objects
 forsI_ 0 hm1 $ \i ->
  forsI_ 0 wm1 $ \j -> do
   cell <- readArray mutableGame (i, j)
   when (cell == Bat || cell == Gem) $
    chain (nextCoord dir) target (i, j) mutableGame

 -- Check outcome
 outcome <- checkOutcome target mutableGame
 finalGame <- unsafeFreeze mutableGame
 pure (mkTotM finalGame, outcome)

{- |
Check the current game outcome.

IMPORTANT INVARIANT: The target position should always contain Air in any valid
game state. No gem, bat, or obstacle should ever occupy the target position.
This invariant is not enforced by this function but is a fundamental requirement
of the game mechanics. Violations may lead to undefined behavior.

Returns:
- Won: when all gems have been collected (gemCount == 0)
- Lost: when a bat occupies the target position
- Running: when the game is still in progress
-}
checkOutcome :: (Int, Int) -> TotS s -> ST s Outcome
checkOutcome target game = do
 targetCell <- readArray game target
 gemCount <- countGems game
 pure $ case targetCell of
  Bat               -> Lost
  _ | gemCount == 0 -> Won    -- all gems collected wins!
  _                 -> Running

-- | Game state for pathfinding
data GameState = GameState
 { gsBoard  :: TotM
 , gsTarget :: (Int, Int)
 } deriving (Eq, Show, Ord)

instance Hashable GameState where
 hashWithSalt s (GameState board target) =
  s `hashWithSalt` board `hashWithSalt` target

-- * Public API Implementation

-- | Create a game state from board and target
mkGameState :: TotM -> (Int, Int) -> GameState
mkGameState = GameState

-- | Apply one gravity direction to the game state
-- Returns either a game-ending result (Left) or new state (Right)
stepGame :: Direction -> GameState -> Either GameException GameState
stepGame dir (GameState board target) =
  case runST (stepGameST dir target board) of
    Left exc       -> Left exc
    Right newBoard -> Right (GameState newBoard target)

-- | Show a game state in human-readable format
showGameState :: GameState -> String
showGameState (GameState board target) = showBoard board target

-- | Get board bounds
getBoardBounds :: TotM -> ((Int, Int), (Int, Int))
getBoardBounds = totMBounds

-- | Get cell at position
getCell :: TotM -> (Int, Int) -> Cell
getCell = totMIndex

-- * Internal Implementation with Exceptions

-- | Internal step function that returns Either for exception handling
stepGameST :: Direction -> (Int, Int) -> TotM -> ST s (Either GameException TotM)
stepGameST dir target board = do
  mutableGame <- thaw (unTotM board)
  result <- applyGravityWithExceptions dir target mutableGame
  case result of
    Left exc -> pure (Left exc)
    Right _ -> do
      finalBoard <- unsafeFreeze mutableGame
      pure (Right (mkTotM finalBoard))

-- | Apply gravity with exception-based outcome detection
applyGravityWithExceptions :: Direction -> (Int, Int) -> TotS s -> ST s (Either GameException ())
applyGravityWithExceptions dir target game = do
  gameBounds <- ST.getBounds game
  let coords = range gameBounds

  -- Process coordinates in the correct order for the gravity direction
  let orderedCoords = orderCoordsForGravity dir coords

  -- Try to move all objects, checking for game-ending conditions
  result <- tryMoveAllObjects orderedCoords
  case result of
    Left exc -> pure (Left exc)
    Right _ -> do
      -- Check if all gems are collected
      gemCount <- countGems game
      if gemCount == 0
        then pure (Left AllGemsCollected)
        else pure (Right ())
  where
    tryMoveAllObjects [] = pure (Right ())
    tryMoveAllObjects (ij:rest) = do
      cell <- readArray game ij
      if cell == Bat || cell == Gem
        then do
          result <- chainWithExceptions (nextCoord dir) target ij game
          case result of
            Left exc -> pure (Left exc)
            Right _  -> tryMoveAllObjects rest
        else tryMoveAllObjects rest

-- | Order coordinates based on gravity direction to avoid conflicts
orderCoordsForGravity :: Direction -> [(Int, Int)] -> [(Int, Int)]
orderCoordsForGravity DirUp coords = reverse coords    -- Process bottom to top
orderCoordsForGravity DirDown coords = coords          -- Process top to bottom
orderCoordsForGravity DirLeft coords =
  -- Process right to left within each row
  let grouped = groupBy (\(r1,_) (r2,_) -> r1 == r2) coords
  in concatMap reverse grouped
orderCoordsForGravity DirRight coords = coords         -- Process left to right-- | Chain movement with exception handling for bat hitting target
chainWithExceptions :: ((Int, Int) -> (Int, Int)) -> (Int, Int) -> (Int, Int) -> TotS s -> ST s (Either GameException ())
chainWithExceptions nextCoordFn target ij0 game = go ij0
  where
    go ij = do
      gameBounds <- ST.getBounds game
      let inBounds = inRange gameBounds
      if not (inBounds ij)
        then pure (Right ())
        else do
          cell <- readArray game ij
          if isAir cell
            then pure (Right ())
            else do
              let ij' = nextCoordFn ij
              if not (inBounds ij')
                then pure (Right ()) -- hit boundary, stop
                else do
                  cell' <- readArray game ij'
                  case cell' of
                    Air -> do
                      -- Move forward
                      writeArray game ij Air
                      -- Check for game-ending conditions BEFORE writing
                      if ij' == target
                        then case cell of
                          Gem -> do
                            writeArray game ij' Air  -- gem disappears
                            pure (Right ())
                          Bat -> pure (Left BatHitTarget)  -- bat hits target = loss
                          _ -> do
                            writeArray game ij' cell
                            go ij'
                        else do
                          writeArray game ij' cell
                          go ij'
                    Obs -> pure (Right ()) -- hit obstacle, stop
                    _ -> do -- hit movable object
                      result <- chainWithExceptions nextCoordFn target ij' game
                      case result of
                        Left exc -> pure (Left exc)
                        Right _ -> do
                          -- Try again - the space might be clear now
                          cell'' <- readArray game ij'
                          if cell'' == Air
                            then do
                              writeArray game ij Air
                              if ij' == target
                                then case cell of
                                  Gem -> do
                                    writeArray game ij' Air
                                    pure (Right ())
                                  Bat -> pure (Left BatHitTarget)
                                  _ -> do
                                    writeArray game ij' cell
                                    go ij'
                                else do
                                  writeArray game ij' cell
                                  go ij'
                            else pure (Right ())

-- | Get all possible next states from current state
neighbors :: GameState -> ForM_ (Direction, GameState)
neighbors (GameState board target) action =
 let directions = [DirUp, DirDown, DirLeft, DirRight] in
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
   Just _ ->
    let (_, moveList, _) = recon dijkResult
    in {-if null moveList
       then Nothing
       else Just moveList-} Just moveList

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
Pretty print a game board using the same format as input.

Displays the board with '*' for target, '@' for gems, '%' for bats,
'#' for obstacles, and '.' for air.
-}
showBoard :: TotM -> (Int, Int) -> String
showBoard board target = unlines $ map showRow [r1..r2]
 where
  ((r1, c1), (r2, c2)) = totMBounds board
  showRow r = [showCell (r, c) | c <- [c1..c2]]
  showCell ij
   | ij == target = '*'
   | otherwise = case totMIndex board ij of
    Air -> '.'
    Bat -> '%'
    Gem -> '@'
    Obs -> '#'
