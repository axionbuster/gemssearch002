{- |
Module      : Dijk
Description : Efficient Dijkstra's shortest path algorithm with move tracking
Copyright   : (c) 2025 axionbuster
License     : BSD-3-Clause
Maintainer  : axionbuster

This module provides a highly efficient implementation of Dijkstra's shortest
path algorithm, specifically designed for uniform-cost search problems where
the target state is unknown but satisfies a predicate.

Key features:

* __Uniform-Cost Search__: Explores states in order of increasing cost until
  finding any state that satisfies a goal predicate
* __Move Tracking__: Stores transition information alongside states, enabling
  direct reconstruction of the move sequence without error-prone reverse
  engineering
* __Flexible State Representation__: Works with any hashable, orderable state
  type
* __Memory Efficient__: Uses priority queues and hashmaps for optimal
  performance

== Example Usage

@
import Dijk

-- Define your state type (must be 'Hashable' and 'Ord')
data GameState = ...
instance Hashable GameState where ...
instance Ord GameState where ...

-- Define your move type
data Move = Up | Down | Left | Right

-- Define neighbor function that yields (move, nextState) pairs
neighbors :: GameState -> 'ForM_' (Move, GameState)
neighbors state action = do
  'forM_' [Up, Down, Left, Right] $ \\move -> do
    case applyMove move state of
      Just nextState -> action (move, nextState)
      Nothing -> return ()

-- Define weight function (cost of transition)
weight :: GameState -> GameState -> Int
weight _ _ = 1  -- uniform cost

-- Define goal predicate
isWon :: GameState -> Bool
isWon state = ...

-- Run the search
let result = 'dijk' weight neighbors isWon startState
case '_dijk'target' result of
  Nothing -> putStrLn "No solution found"
  Just _ -> do
    let (statePath, moveSequence, totalCost) = 'recon' result
    putStrLn $ "Solution: " ++ show moveSequence
@
-}
module Dijk (ForM_, Dijk, _dijk'target, dijk, recon) where

import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Hashable
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as M
import qualified Data.HashPSQ               as Q

-- node information:
--
-- either start node or step node (cost + predecessor + how)
--
-- ordering the more common (almost always `Step`) constructor first. known to
-- improve performance across the board to order constructors by frequency.
data Node k h = Step Int k h | Start
 deriving (Show)

{- |
A polymorphic container type for neighbor enumeration.

This type represents a computation that applies an action to each element
in some container-like structure. It's more flexible than lists because:

* The container can be monomorphic (e.g., specialized data structures)
* The container can be abstract (e.g., generated on-demand)
* The container can be infinite (though search assumes it's finite)
* The container can be empty

== Usage

@
-- From a list
neighbors :: [a] -> ForM_ a
neighbors xs action = forM_ xs action

-- From a specialized container
neighbors :: MyContainer a -> ForM_ a
neighbors container action =
  forEachInContainer container action

-- Generated on-demand
neighbors :: State -> ForM_ (Move, State)
neighbors state action =
  forM_ allMoves $ \\move ->
    case applyMove move state of
      Just nextState -> action (move, nextState)
      Nothing -> return ()
@
-}
type ForM_ a = forall m b. (Monad m) => (a -> m b) -> m ()

{- |
The result of running Dijkstra's algorithm.

This opaque type contains the search results including the cost table and
the target state (if found). Use '_dijk'target' to check if a target was
found, and 'recon' to reconstruct the path and move sequence.

Type parameters:

* @k@: State type (must be 'Hashable' and 'Ord')
* @h@: Move/transition type (can be any type, including '()')

The internal structure is optimized for efficient lookups and path
reconstruction, but should not be accessed directly.
-}
data Dijk k h = Dijk (HashMap k (Node k h)) (Maybe k)
 deriving
  ( Show -- ^ Debugging accommodation
  )

{- |
Extract the target state found by the search, if any.

Returns 'Just' the first state that satisfied the goal predicate (in order
of increasing cost), or 'Nothing' if no such state exists or is reachable.

== Example

@
let result = 'dijk' weight neighbors isGoal startState
case '_dijk'target' result of
  Nothing -> putStrLn "No solution exists"
  Just targetState -> do
    let (path, moves, cost) = 'recon' result targetState
    putStrLn $ "Found solution with cost " ++ show cost
@
-}
_dijk'target :: Dijk k h -> Maybe k
_dijk'target ~(Dijk _ t) = t

lookupCost :: (Hashable k) => k -> HashMap k (Node k h) -> Int
lookupCost k m = case M.lookup k m of
 Just Start           -> 0
 Just (Step cost _ _) -> cost
 Nothing              -> maxBound
{-# INLINE lookupCost #-}

{- |
Run Dijkstra's shortest path algorithm to find the cheapest path to any
state satisfying a goal predicate.

This is a /uniform-cost search/ that explores states in order of increasing
path cost until finding the first state that satisfies the goal predicate.
The algorithm is __optimal__: the first solution found is guaranteed to have
minimum cost.

== Parameters

* @weight@: Cost function for transitions. Must be non-negative.
  @weight from to@ returns the cost of moving from state @from@ to state @to@.

* @neighbors@: Function that enumerates all possible transitions from a
  state. Should call the action with @(move, nextState)@ pairs for each
  valid transition. The @move@ value is stored and can be retrieved later.

* @stop@: Goal predicate. @stop state@ returns 'True' if @state@ satisfies
  the goal condition.

* @start@: Initial state to begin the search from.

== Returns

A 'Dijk' result containing the search tree. Use '_dijk'target' to check if
a solution exists and 'recon' to reconstruct the optimal path.

== Complexity

* Time: /O((V + E) log V)/ where /V/ is the number of reachable states and
  /E/ is the number of transitions
* Space: /O(V)/ to store the search tree

== Example

@
-- Simple grid pathfinding
weight :: (Int,Int) -> (Int,Int) -> Int
weight _ _ = 1  -- all moves cost 1

neighbors :: (Int,Int) -> 'ForM_' (Direction, (Int,Int))
neighbors (x,y) action = do
  let moves = [(Up, (x,y-1)), (Down, (x,y+1)),
               (Left, (x-1,y)), (Right, (x+1,y))]
  forM_ moves $ \\(dir, pos) ->
    when (inBounds pos && not (isWall pos)) $
      action (dir, pos)

isGoal :: (Int,Int) -> Bool
isGoal pos = pos == targetPosition

let result = 'dijk' weight neighbors isGoal startPosition
@

== Notes

* The algorithm stops immediately when the first goal state is found
* States are explored in order of increasing path cost (uniform-cost search)
* The @weight@ function must return non-negative values for correctness
* Cycles and revisits are handled efficiently via the priority queue
-}
dijk
 :: (Hashable k, Ord k)
 => (k -> k -> Int) -- ^ Cost function (non-negative)
 -> (k -> ForM_ (h, k)) -- ^ Neighbor enumeration with move labels
 -> (k -> Bool) -- ^ Goal predicate
 -> k -- ^ Start state
 -> Dijk k h -- ^ Search result
dijk weight neighbors stop start = entry where
 -- the starting vertex has no predecessor
 entry = evalState go (Q.singleton start 0 (), M.singleton start Start)
 go = do
  (bq0, costs) <- get
  case Q.minView bq0 of
   Just (ux, _, _, _) | stop ux -> (`Dijk` Just ux) <$!> gets snd
   Just (ux, ud, _, bq1) -> do
    put (bq1, costs)
    neighbors ux $ \(how, vx) -> do
     (bq2, costs2) <- get
     let vd0 = lookupCost vx costs2
         vd1 = ud + weight ux vx
     when (vd1 < vd0) $ put
      ( Q.insert vx vd1 () bq2
      , M.insert vx (Step vd1 ux how) costs2
      )
    go
   Nothing -> (`Dijk` Nothing) <$!> gets snd
{-# INLINE dijk #-}

{- |
Reconstruct the optimal path from start to the target state found by the search.

Given a 'Dijk' search result, reconstructs the shortest path from the original
start state to the target state that was found during the search. Returns the
sequence of intermediate states, the sequence of moves, and the total path cost.

== Parameters

* @dijkResult@: The result from running 'dijk'

== Returns

A tuple @(statePath, moveSequence, totalCost)@ where:

* @statePath@: List of intermediate states from start to target (excluding
  the start state, including the target state)
* @moveSequence@: List of moves that transform the start state into the
  target state via the intermediate states
* @totalCost@: Total cost of the path

== Example

@
let result = 'dijk' weight neighbors isGoal startState
case '_dijk'target' result of
  Just _ -> do
    let (path, moves, cost) = 'recon' result
    putStrLn $ "Path: " ++ show (startState : path)
    putStrLn $ "Moves: " ++ show moves
    putStrLn $ "Cost: " ++ show cost
  Nothing -> putStrLn "No path found"
@

== Complexity

* Time: /O(L)/ where /L/ is the length of the path
* Space: /O(L)/ for the returned lists

== Notes

* If no target state was found during the search, returns empty lists
  and cost 'maxBound'
* The move sequence can be applied in order to transform the start state
  into the target state
* The state path excludes the start state but includes the target state
-}

recon :: (Hashable k) => Dijk k h -> ([k], [h], Int)
recon (Dijk _      Nothing) = ([], [], maxBound)
recon (Dijk costs (Just target)) = entry where
 entry = case costs M.! target of
  Step c k h -> case go k [target] [h] of
   (ks, hs) -> (ks, hs, c)
  Start -> ([], [], 0)
 go k ks hs = case costs M.! k of
  Step _ k' h' -> go k' (k : ks) (h' : hs)
  Start        -> (k : ks, hs)
{-# INLINE recon #-}
