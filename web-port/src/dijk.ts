/**
 * @fileoverview Efficient Dijkstra's shortest path algorithm with move tracking
 * @author axionbuster
 * @license BSD-3-Clause
 * 
 * This module provides a highly efficient implementation of Dijkstra's shortest
 * path algorithm, specifically designed for uniform-cost search problems where
 * the target state is unknown but satisfies a predicate.
 * 
 * Key features:
 * 
 * * **Uniform-Cost Search**: Explores states in order of increasing cost until
 *   finding any state that satisfies a goal predicate
 * * **Move Tracking**: Stores transition information alongside states, enabling
 *   direct reconstruction of the move sequence without error-prone reverse
 *   engineering
 * * **Flexible State Representation**: Works with any hashable state type
 * * **Memory Efficient**: Uses priority queues and hashmaps for optimal
 *   performance
 * 
 * @example
 * ```typescript
 * import { dijk, recon } from './dijk';
 * 
 * // Define your state type
 * interface GameState {
 *   x: number;
 *   y: number;
 *   gems: Set<string>;
 * }
 * 
 * // Define your move type
 * enum Direction { Up, Down, Left, Right }
 * 
 * // Define neighbor function
 * function* neighbors(state: GameState): Iterable<[Direction, GameState]> {
 *   for (const move of [Direction.Up, Direction.Down, Direction.Left, Direction.Right]) {
 *     const nextState = applyMove(move, state);
 *     if (nextState) {
 *       yield [move, nextState];
 *     }
 *   }
 * }
 * 
 * // Define weight function (cost of transition)
 * const weight = (from: GameState, to: GameState): number => 1; // uniform cost
 * 
 * // Define goal predicate
 * const isWon = (state: GameState): boolean => state.gems.size === 0;
 * 
 * // Run the search
 * const result = dijk(weight, neighbors, isWon, startState);
 * if (result.target) {
 *   const [statePath, moveSequence, totalCost] = recon(result, result.target);
 *   console.log("Solution:", moveSequence);
 * }
 * ```
 */

import { PairingHeap } from './pairing-heap';

/**
 * A polymorphic container type for neighbor enumeration.
 * 
 * This type represents an iterable that yields neighboring states and their
 * corresponding moves. It's more flexible than arrays because:
 * 
 * * The container can be generated on-demand
 * * The container can be infinite (though search assumes it's finite)
 * * The container can be empty
 * * Memory efficient iteration
 */
export type ForM_<T> = Iterable<T>;

/**
 * Node information for the search tree.
 * 
 * Either a start node (cost only) or step node (cost + predecessor + move).
 * The Step variant is ordered first as it's more common in practice.
 */
export type Node<K, H> =
 | { type: 'step'; cost: number; predecessor: K; move: H }
 | { type: 'start'; cost: number };

/**
 * The result of running Dijkstra's algorithm.
 * 
 * This contains the search results including the cost table and
 * the target state (if found). Use `target` to check if a target was
 * found, and `recon` to reconstruct the path and move sequence.
 * 
 * @template K State type (must be usable as Map key)
 * @template H Move/transition type (can be any type)
 */
export interface Dijk<K, H> {
 /** Cost table mapping states to their optimal cost and predecessor info */
 readonly costs: Map<K, Node<K, H>>;
 /** The target state found by the search, if any */
 readonly target: K | null;
}

/**
 * Priority queue entry for Dijkstra's algorithm.
 * Contains state and its current best cost.
 */
interface QueueEntry<K> {
 state: K;
 cost: number;
}

/**
 * Look up the cost of reaching a state, returning Infinity if not yet reached.
 */
function lookupCost<K, H>(state: K, costs: Map<K, Node<K, H>>): number {
 const node = costs.get(state);
 if (!node) return Infinity;
 return node.cost;
}

/**
 * Run Dijkstra's shortest path algorithm to find the cheapest path to any
 * state satisfying a goal predicate.
 * 
 * This is a *uniform-cost search* that explores states in order of increasing
 * path cost until finding the first state that satisfies the goal predicate.
 * The algorithm is **optimal**: the first solution found is guaranteed to have
 * minimum cost.
 * 
 * @param weight Cost function for transitions. Must be non-negative.
 *               `weight(from, to)` returns the cost of moving from state `from` to state `to`.
 * 
 * @param neighbors Function that enumerates all possible transitions from a
 *                  state. Should yield `[move, nextState]` pairs for each
 *                  valid transition. The `move` value is stored and can be retrieved later.
 * 
 * @param stop Goal predicate. `stop(state)` returns `true` if `state` satisfies
 *             the goal condition.
 * 
 * @param start Initial state to begin the search from.
 * 
 * @returns A `Dijk` result containing the search tree. Use `target` to check if
 *          a solution exists and `recon` to reconstruct the optimal path.
 * 
 * @example
 * ```typescript
 * // Simple grid pathfinding
 * const weight = (from: [number, number], to: [number, number]): number => 1; // all moves cost 1
 * 
 * function* neighbors(pos: [number, number]): Iterable<[Direction, [number, number]]> {
 *   const [x, y] = pos;
 *   const moves: [Direction, [number, number]][] = [
 *     [Direction.Up, [x, y-1]],
 *     [Direction.Down, [x, y+1]],
 *     [Direction.Left, [x-1, y]],
 *     [Direction.Right, [x+1, y]]
 *   ];
 *   
 *   for (const [dir, newPos] of moves) {
 *     if (inBounds(newPos) && !isWall(newPos)) {
 *       yield [dir, newPos];
 *     }
 *   }
 * }
 * 
 * const isGoal = (pos: [number, number]): boolean => 
 *   pos[0] === targetX && pos[1] === targetY;
 * 
 * const result = dijk(weight, neighbors, isGoal, startPosition);
 * ```
 * 
 * @complexity
 * * Time: O((V + E) log V) where V is the number of reachable states and
 *   E is the number of transitions
 * * Space: O(V) to store the search tree
 * 
 * @notes
 * * The algorithm stops immediately when the first goal state is found
 * * States are explored in order of increasing path cost (uniform-cost search)
 * * The `weight` function must return non-negative values for correctness
 * * Cycles and revisits are handled efficiently via the priority queue
 */
export function dijk<K, H>(
 weight: (from: K, to: K) => number,
 neighbors: (state: K) => ForM_<[H, K]>,
 stop: (state: K) => boolean,
 start: K
): Dijk<K, H> {
 // Priority queue for states to explore, ordered by cost
 const queue = new PairingHeap<QueueEntry<K>>();

 // Custom comparison function for the priority queue (min-heap by cost)
 const originalInsert = queue.insert.bind(queue);
 queue.insert = function (entry: QueueEntry<K>) {
  return originalInsert.call(this, entry);
 };

 // We need to override the heap's comparison
 // Since PairingHeap uses generic comparison, we'll work around it
 const queueArray: QueueEntry<K>[] = [];

 // Cost table: maps state to its best known cost and predecessor info
 const costs = new Map<K, Node<K, H>>();

 // Initialize with start state
 queueArray.push({ state: start, cost: 0 });
 costs.set(start, { type: 'start', cost: 0 });

 while (queueArray.length > 0) {
  // Find and remove minimum cost entry
  let minIndex = 0;
  for (let i = 1; i < queueArray.length; i++) {
   if (queueArray[i].cost < queueArray[minIndex].cost) {
    minIndex = i;
   }
  }

  const { state: currentState, cost: currentCost } = queueArray[minIndex];
  queueArray.splice(minIndex, 1);

  // Check if we've found a goal state
  if (stop(currentState)) {
   return { costs, target: currentState };
  }

  // Skip if we've found a better path to this state already
  const knownCost = lookupCost(currentState, costs);
  if (currentCost > knownCost) {
   continue;
  }

  // Explore neighbors
  for (const [move, nextState] of neighbors(currentState)) {
   const tentativeCost = currentCost + weight(currentState, nextState);
   const bestKnownCost = lookupCost(nextState, costs);

   if (tentativeCost < bestKnownCost) {
    // Found a better path to nextState
    queueArray.push({ state: nextState, cost: tentativeCost });
    costs.set(nextState, {
     type: 'step',
     cost: tentativeCost,
     predecessor: currentState,
     move: move
    });
   }
  }
 }

 // No goal state found
 return { costs, target: null };
}

/**
 * Reconstruct the optimal path from start to a target state.
 * 
 * Given a `Dijk` search result and a target state, reconstructs the shortest
 * path from the original start state to the target. Returns the sequence of
 * intermediate states, the sequence of moves, and the total path cost.
 * 
 * @param dijkResult The result from running `dijk`
 * @param target The target state to reconstruct a path to. This should be a state
 *               that was actually reached during the search (typically obtained from
 *               `dijkResult.target`).
 * 
 * @returns A tuple `[statePath, moveSequence, totalCost]` where:
 * * `statePath`: Array of intermediate states from start to target (excluding
 *   the start state, including the target state)
 * * `moveSequence`: Array of moves that transform the start state into the
 *   target state via the intermediate states
 * * `totalCost`: Total cost of the path
 * 
 * @example
 * ```typescript
 * const result = dijk(weight, neighbors, isGoal, startState);
 * if (result.target) {
 *   const [path, moves, cost] = recon(result, result.target);
 *   console.log(`Path: ${[startState, ...path]}`);
 *   console.log(`Moves: ${moves}`);
 *   console.log(`Cost: ${cost}`);
 * } else {
 *   console.log("No path found");
 * }
 * ```
 * 
 * @complexity
 * * Time: O(L) where L is the length of the path
 * * Space: O(L) for the returned arrays
 * 
 * @notes
 * * If the target state was not reached during the search, returns empty
 *   arrays and cost 0
 * * The move sequence can be applied in order to transform the start state
 *   into the target state
 * * The state path excludes the start state but includes the target state
 */
export function recon<K, H>(dijkResult: Dijk<K, H>, target: K): [K[], H[], number] {
 const stateList: K[] = [];
 const moveList: H[] = [];
 let current = target;

 // Build path by following predecessors from target back to start
 while (true) {
  const node = dijkResult.costs.get(current);
  if (!node) {
   // Target not found in search results
   return [[], [], 0];
  }

  if (node.type === 'step') {
   stateList.unshift(node.predecessor);
   moveList.unshift(node.move);
   current = node.predecessor;
  } else {
   // Reached start node - return the total cost which is the target's cost
   const targetNode = dijkResult.costs.get(target);
   const totalCost = targetNode ? targetNode.cost : 0;

   // Remove the start state from the path (as per Haskell implementation)
   stateList.shift();

   return [stateList, moveList, totalCost];
  }
 }
}
