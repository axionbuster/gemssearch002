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
   GameState(..)         -- Opaque, only for step-by-step operation
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

data Direction = DirUp | DirDown | DirLeft | DirRight
 deriving (Eq, Show, Enum, Bounded)

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
stepGameST :: Direction -> (Int, Int) -> TotM
           -> ST s (Either GameException TotM)
stepGameST dir target board = do
 mutableGame <- thaw (unTotM board)
 result <- applyGravityWithExceptions dir target mutableGame
 case result of
  Left exc -> pure (Left exc)
  Right _ -> do
   finalBoard <- unsafeFreeze mutableGame
   pure (Right (mkTotM finalBoard))

-- | Apply gravity with exception-based outcome detection
applyGravityWithExceptions :: Direction -> (Int, Int) -> TotS s
                           -> ST s (Either GameException ())
applyGravityWithExceptions dir target game = do
 bnds <- ST.getBounds game
 let coords = range bnds

 -- Process coordinates in the correct order for the gravity direction
 let ocs = orderCoordsForGravity dir coords

 -- Try to move all objects, checking for game-ending conditions
 result <- tryMoveAllObjects ocs
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
orderCoordsForGravity DirRight coords = coords         -- Process left to right

-- | Chain movement with exception handling for bat hitting target
-- | Chain movement with exception handling for bat hitting target
chainWithExceptions :: ((Int, Int) -> (Int, Int)) -> (Int, Int) -> (Int, Int)
                 -> TotS s -> ST s (Either GameException ())
chainWithExceptions nf target ij0 game = go ij0
 where
  go ij = do
   bnds <- ST.getBounds game
   let inBounds = inRange bnds
   if not (inBounds ij)
    then pure (Right ())
    else do
     cell <- readArray game ij
     if isAir cell
      then pure (Right ())
      else do
       let ij' = nf ij
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
           result <- chainWithExceptions nf target ij' game
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
neighbors st action =
 let directions = [DirUp, DirDown, DirLeft, DirRight] in
 forM_ directions $ \dir -> do
  case stepGame dir st of
   Left AllGemsCollected -> pure ()  -- Don't explore further - terminal win
   Left BatHitTarget     -> pure ()  -- Don't include losing states in search
   Right newState        -> void $ action (dir, newState)  -- Continue exploring

-- | Check if this state can lead to a win in one move
isWinnable :: GameState -> Bool
isWinnable st =
 let directions = [DirUp, DirDown, DirLeft, DirRight]
 in any (\dir -> case stepGame dir st of
                  Left AllGemsCollected -> True
                  _                     -> False) directions

{- |
Solve the gem seeker puzzle using uniform-cost search.

Finds the optimal sequence of gravity changes to collect all gems and reach
the target, or returns Nothing if no solution exists.
-}
solve :: GameState -> Maybe [Direction]
solve start =
 let
  weight _ _ = 1  -- each move costs 1

  dr = dijk weight neighbors isWinnable start
  mws = _dijk'target dr
 in
   case mws of
   Nothing -> Nothing  -- No winning state found
   Just wst ->
    let (_, mvs, _) = recon dr
        -- Find the direction that leads to AllGemsCollected from the win state
        wdirs = [dir | dir <- [DirUp, DirDown, DirLeft, DirRight],
                       case stepGame dir wst of
                         Left AllGemsCollected -> True
                         _                     -> False]
    in case wdirs of
         []     -> Nothing  -- Shouldn't happen, but safety first
         (wd:_) -> Just (mvs ++ [wd]){- |
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
  bnds = ((0, 0), (rows - 1, cols - 1))
  pairs =
   [ ((r, c), cell)
   | (r, row) <- zip [0..] cells, (c, cell) <- zip [0..] row
   ]
  board = array bnds pairs
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
