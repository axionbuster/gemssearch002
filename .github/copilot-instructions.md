# Gem Seeker Solver - AI Coding Instructions

This is a Haskell-based solver for the Gem Seeker minigame from Tomb of the Mask+, featuring a multi-language architecture with TypeScript web ports and Next.js visualization tools.

## Core Architecture

### Haskell Solver Components
- **`TotM.hs`**: Main game engine using exception-based control flow (`GameException`) and mutable arrays. Uses pattern synonyms for cell types (`Air`, `Bat`, `Gem`, `Obs`) with 2-bit encoding.
- **`TotM2.hs`**: Optimized bit-packed version storing 4 cells per Word for memory efficiency. Uses `quotRem4` for bit manipulation.
- **`Dijk.hs`**: Generic Dijkstra's algorithm with move tracking - designed for uniform-cost search where target satisfies a predicate.
- **`SolveTotM2.hs`**: High-level solver interface combining the optimized game state with pathfinding.

### Game Logic Patterns
- **Control Flow**: Uses exceptions (`BatHitTarget`, `AllGemsCollected`) for game state transitions, NOT error handling
- **State Representation**: Immutable `UArray (Int, Int) Cell` for boards with `(row, col)` indexing
- **Physics Simulation**: All movable objects slide until hitting obstacles/boundaries when gravity changes
- **Win/Lose Logic**: Gems disappear when reaching target; bats cause immediate loss at target

## Development Workflows

### Build & Test Commands
```bash
stack build                    # Default build
stack test                     # Run all hspec tests
stack exec gemssearch002-exe   # Run solver on stdin
```

### Performance Analysis
- Uses `-ddump-simpl` for GHC optimization analysis (`.dyn.dump-simpl` files)
- Profiling builds available but commented out in `package.yaml`
- Watch mode: `stack build --test --no-run-tests --file-watch`

### Testing Patterns
- **hspec-discover**: Auto-discovers `*Spec.hs` files in `test/` directory
- **Exception Testing**: Tests check for specific `GameException` types rather than boolean outcomes
- **Integration Tests**: Located in `IntegrationSpec.hs` and `OriginalSpec.hs`
- **Physics Tests**: `PhysicsSpec.hs` validates core game mechanics

## Project-Specific Conventions

### Haskell Code Style
- **Strict Evaluation**: Uses `Strict` language extension globally
- **GHC2024**: Modern Haskell with latest language features
- **Newtype Wrappers**: `TotM` wraps `UArray`, `IA`/`SA` for immutable/mutable arrays
- **Pattern Synonyms**: Prefer `pattern Air` over raw bit values
- **Import Qualified**: Heavy use of qualified imports (e.g., `Data.Array.ST as ST`)

### Cross-Language Integration
- **TypeScript Port**: `web-port/src/totm.ts` mirrors Haskell types and algorithms
- **Next.js Visualization**: `web-port/app/` contains React components for puzzle display
- **Shared Types**: Cell types use identical bit patterns across languages (`0b00` = Air, etc.)

### Data Structures
- **Bit Packing**: `TotM2.hs` packs 4 cells per Word using `quotRem4` and bit shifting
- **Priority Queues**: Uses `psqueues` library for Dijkstra implementation
- **Hashable Everything**: All game states implement `Hashable` for efficient deduplication

## Key Files for Understanding
- `README.md`: Problem description and game mechanics
- `src/TotM.hs`: Core API and game state management
- `src/Dijk.hs`: Pathfinding algorithm with detailed documentation
- `test/PhysicsSpec.hs`: Examples of game mechanics testing
- `web-port/src/totm.ts`: TypeScript equivalent of core logic
- `web-port/app/src/types/puzzle.ts`: React component type definitions

## External Dependencies
- **Haskell**: `psqueues` for priority queues, `array` for mutable arrays, `hashable` for state deduplication
- **TypeScript**: Rollup for bundling, Jest for testing
- **Next.js**: Radix UI components, Tailwind CSS for styling

When working on this project, understand that performance is critical - the solver handles complex multi-gem puzzles efficiently through bit packing and optimized data structures.