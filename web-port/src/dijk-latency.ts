/**
 * @fileoverview Latency-optimized Dijkstra for game scenarios
 * @author axionbuster
 * @license BSD-3-Clause
 * 
 * Optimized for small-medium state spaces (dozens to tens of thousands)
 * with fixed branching factor of 4, prioritizing minimal latency.
 */

import { Node, Dijk } from './dijk';

/**
 * Four-way neighbor enumeration optimized for games with 4 directional moves.
 * Returns exactly 4 potential next states, with null indicating invalid moves.
 */
export type QuadNeighbors<K> = readonly [K | null, K | null, K | null, K | null];

/**
 * Ultra-fast Dijkstra implementation optimized for:
 * - Small to medium state spaces (< 50k states)
 * - Exactly 4 possible moves per state
 * - Latency-critical applications (games, interactive solvers)
 * 
 * This eliminates all iterator overhead by using direct array access
 * and unrolled loops for the fixed branching factor.
 */
export function dijkLatency<K, H>(
 weight: (from: K, to: K) => number,
 neighbors: (state: K) => QuadNeighbors<K>,
 moveLabels: readonly [H, H, H, H], // [move0, move1, move2, move3] 
 stop: (state: K) => boolean,
 start: K
): Dijk<K, H> {
 // Pre-allocate arrays to minimize GC pressure
 const queue: Array<{ state: K; cost: number }> = [];
 const costs = new Map<K, Node<K, H>>();

 // Optimized priority queue for small workloads
 const insertQueue = (entry: { state: K; cost: number }) => {
  const len = queue.length;
  if (len === 0) {
   queue.push(entry);
   return;
  }

  // Simple insertion for small queues (faster than heap for < 1000 elements)
  let insertIndex = len;
  for (let i = len - 1; i >= 0; i--) {
   if (queue[i].cost <= entry.cost) {
    insertIndex = i + 1;
    break;
   }
   insertIndex = i;
  }
  queue.splice(insertIndex, 0, entry);
 };

 const extractMin = (): { state: K; cost: number } | null => {
  return queue.shift() || null;
 };

 // Initialize
 insertQueue({ state: start, cost: 0 });
 costs.set(start, { type: 'start', cost: 0 });

 while (queue.length > 0) {
  const current = extractMin();
  if (!current) break;

  const { state: currentState, cost: currentCost } = current;

  // Early termination
  if (stop(currentState)) {
   return { costs, target: currentState };
  }

  // Skip if we found a better path already
  const knownNode = costs.get(currentState);
  if (!knownNode || currentCost > knownNode.cost) {
   continue;
  }

  // Unrolled neighbor processing for exactly 4 moves
  const [next0, next1, next2, next3] = neighbors(currentState);

  // Process neighbor 0
  if (next0 !== null) {
   const tentativeCost = currentCost + weight(currentState, next0);
   const knownCost = costs.get(next0)?.cost ?? Infinity;

   if (tentativeCost < knownCost) {
    insertQueue({ state: next0, cost: tentativeCost });
    costs.set(next0, {
     type: 'step',
     cost: tentativeCost,
     predecessor: currentState,
     move: moveLabels[0]
    });
   }
  }

  // Process neighbor 1
  if (next1 !== null) {
   const tentativeCost = currentCost + weight(currentState, next1);
   const knownCost = costs.get(next1)?.cost ?? Infinity;

   if (tentativeCost < knownCost) {
    insertQueue({ state: next1, cost: tentativeCost });
    costs.set(next1, {
     type: 'step',
     cost: tentativeCost,
     predecessor: currentState,
     move: moveLabels[1]
    });
   }
  }

  // Process neighbor 2
  if (next2 !== null) {
   const tentativeCost = currentCost + weight(currentState, next2);
   const knownCost = costs.get(next2)?.cost ?? Infinity;

   if (tentativeCost < knownCost) {
    insertQueue({ state: next2, cost: tentativeCost });
    costs.set(next2, {
     type: 'step',
     cost: tentativeCost,
     predecessor: currentState,
     move: moveLabels[2]
    });
   }
  }

  // Process neighbor 3
  if (next3 !== null) {
   const tentativeCost = currentCost + weight(currentState, next3);
   const knownCost = costs.get(next3)?.cost ?? Infinity;

   if (tentativeCost < knownCost) {
    insertQueue({ state: next3, cost: tentativeCost });
    costs.set(next3, {
     type: 'step',
     cost: tentativeCost,
     predecessor: currentState,
     move: moveLabels[3]
    });
   }
  }
 }

 return { costs, target: null };
}

/**
 * Convenience wrapper for common 4-directional movement games.
 * Assumes moves are ordered: [Up, Down, Left, Right]
 */
export function dijkFourWay<K>(
 weight: (from: K, to: K) => number,
 neighbors: (state: K) => QuadNeighbors<K>,
 stop: (state: K) => boolean,
 start: K
): Dijk<K, 'Up' | 'Down' | 'Left' | 'Right'> {
 return dijkLatency(
  weight,
  neighbors,
  ['Up', 'Down', 'Left', 'Right'] as const,
  stop,
  start
 );
}
