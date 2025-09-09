# Dijkstra's Algorithm TypeScript Port

This is a TypeScript port of the efficient Dijkstra's shortest path algorithm from the original Haskell `Dijk.hs` module, specifically designed for uniform-cost search problems in game solving applications.

## Overview

The original Haskell implementation was designed for the Gem Seeker solver (a Tomb of the Mask+ variant). This TypeScript port maintains the same API design philosophy and performance characteristics while adapting to JavaScript/TypeScript idioms.

## Key Features

- **Uniform-Cost Search**: Explores states in order of increasing cost until finding any state that satisfies a goal predicate
- **Move Tracking**: Stores transition information alongside states, enabling direct reconstruction of the move sequence
- **Flexible State Representation**: Works with any state type that can be used as a Map key
- **Memory Efficient**: Uses priority queues and Maps for optimal performance
- **Type Safe**: Full TypeScript support with generics

## Quick Start

```typescript
import { dijk, recon } from './dijk';

// Define your state and move types
type Position = [number, number];
enum Direction { Up, Down, Left, Right }

// Define the search parameters
const weight = (from: Position, to: Position): number => 1; // uniform cost
const isGoal = (pos: Position): boolean => pos[0] === 2 && pos[1] === 2;

function* neighbors(pos: Position): Iterable<[Direction, Position]> {
  const [x, y] = pos;
  const moves: [Direction, Position][] = [
    [Direction.Up, [x, y-1]],
    [Direction.Down, [x, y+1]],
    [Direction.Left, [x-1, y]],
    [Direction.Right, [x+1, y]]
  ];
  
  for (const [dir, newPos] of moves) {
    if (inBounds(newPos) && !isWall(newPos)) {
      yield [dir, newPos];
    }
  }
}

// Run the search
const result = dijk(weight, neighbors, isGoal, startPosition);

if (result.target) {
  const [path, moves, cost] = recon(result, result.target);
  console.log(`Solution: ${moves} (cost: ${cost})`);
}
```

## API Reference

### Core Functions

#### `dijk<K, H>(weight, neighbors, stop, start): Dijk<K, H>`

Runs Dijkstra's algorithm to find the shortest path to any state satisfying a goal predicate.

**Parameters:**
- `weight: (from: K, to: K) => number` - Cost function for transitions (must be non-negative)
- `neighbors: (state: K) => Iterable<[H, K]>` - Function that yields valid transitions as `[move, nextState]` pairs
- `stop: (state: K) => boolean` - Goal predicate returning true for target states
- `start: K` - Initial state to begin search from

**Returns:** `Dijk<K, H>` - Search result containing cost table and target state (if found)

#### `recon<K, H>(dijkResult, target): [K[], H[], number]`

Reconstructs the optimal path from start to target state.

**Parameters:**
- `dijkResult: Dijk<K, H>` - Result from running `dijk`
- `target: K` - Target state to reconstruct path to

**Returns:** Tuple of `[statePath, moveSequence, totalCost]`

### Type Definitions

#### `ForM_<T>`
```typescript
type ForM_<T> = Iterable<T>
```
A polymorphic container type for neighbor enumeration. More flexible than arrays as it can be generated on-demand.

#### `Node<K, H>`
```typescript
type Node<K, H> = 
  | { type: 'step'; cost: number; predecessor: K; move: H }
  | { type: 'start'; cost: number }
```
Internal node representation storing either start information or step information with predecessor.

#### `Dijk<K, H>`
```typescript
interface Dijk<K, H> {
  readonly costs: Map<K, Node<K, H>>;
  readonly target: K | null;
}
```
Search result containing the cost table and target state (if found).

## Examples

### Simple Grid Pathfinding

```typescript
function runGridExample() {
  type Position = [number, number];
  enum Direction { Up, Down, Left, Right }
  
  const weight = (from: Position, to: Position): number => 1;
  
  function* neighbors(pos: Position): Iterable<[Direction, Position]> {
    // ... generate valid adjacent positions
  }
  
  const isGoal = (pos: Position): boolean => 
    pos[0] === targetX && pos[1] === targetY;
  
  const result = dijk(weight, neighbors, isGoal, startPos);
  
  if (result.target) {
    const [path, moves, cost] = recon(result, result.target);
    console.log(`Found path: ${moves.map(d => Direction[d]).join(' → ')}`);
  }
}
```

### Game State Search

```typescript
interface GameState {
  position: Position;
  inventory: Set<string>;
  health: number;
}

function runGameExample() {
  const weight = (from: GameState, to: GameState): number => 1;
  
  function* neighbors(state: GameState): Iterable<[Action, GameState]> {
    // ... generate valid game actions and resulting states
  }
  
  const isWon = (state: GameState): boolean => 
    state.inventory.has('treasure') && isAtExit(state.position);
  
  const result = dijk(weight, neighbors, isWon, initialState);
}
```

## Performance

- **Time Complexity**: O((V + E) log V) where V is reachable states, E is transitions
- **Space Complexity**: O(V) for the search tree
- **Optimizations**: Min-heap priority queue, efficient state representation

## Comparison with Original Haskell

The TypeScript port maintains the same algorithmic structure as the original Haskell implementation:

| Aspect | Haskell | TypeScript |
|--------|---------|------------|
| Priority Queue | HashPSQ | Array-based min-heap (simple implementation) |
| Hash Map | HashMap.Strict | Map |
| Generics | Type parameters | TypeScript generics |
| Neighbor enumeration | ForM_ | Iterable |
| Performance | Excellent | Good (could be improved with better heap) |

## Future Improvements

1. **Better Priority Queue**: Replace the simple array-based implementation with the existing PairingHeap for better performance
2. **State Hashing**: Add support for custom state hashing/equality for complex state types
3. **Bidirectional Search**: Add A* or bidirectional search variants
4. **Memory Optimization**: Implement state pruning for large search spaces

## Testing

Run the test suite:
```bash
npm run test
```

Run the demo:
```bash
npm run build
node dist/demo.js
```

## License

BSD-3-Clause (same as original project)
