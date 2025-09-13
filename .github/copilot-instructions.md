# Gem Seeker Solver - AI Coding Instructions

This is a Haskell-based solver for the Gem Seeker minigame from Tomb of the Mask+.

## Core Architecture

### Haskell Solver Components
- **`TotM2.hs`**: Optimized bit-packed version storing 16 or 32 cells per Word - two bits each - for memory efficiency. Uses `quotRemWord` for bit manipulation.
- **`Dijk.hs`**: Generic Dijkstra's algorithm with move tracking - designed for uniform-cost search where target satisfies a predicate.
- **`SolveTotM2.hs`**: High-level solver interface combining the optimized game state with pathfinding.

### Game Logic Patterns
- **Control Flow**: Uses exceptions for signaling game over
- **State Representation**: Two-dimensional arrays of packed bits, representing cells
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

## Project-Specific Conventions

### Haskell Code Style
- **Strict Evaluation**: Uses `Strict` language extension globally
- **GHC2024**: Modern Haskell with latest language features
- **Newtype Wrappers**: `TotM2` wraps `UArray`, `IA`/`SA` for immutable/mutable arrays
- **Pattern Synonyms**: Prefer `pattern Air` over raw bit values

### Data Structures
- **Bit Packing**: `TotM2.hs` packs 16 or 32 cells per Word using `quotRemWord` and bit shifting
- **Priority Queues**: Uses `psqueues` library for Dijkstra implementation
- **Hashable Game States**: All game states implement `Hashable` for efficient deduplication

## Key Files for Understanding
- `README.md`: Problem description and game mechanics
- `src/TotM2.hs`: Core API and game state management
- `src/SolveTotM2.hs`: Solver API
- `src/Dijk.hs`: Pathfinding algorithm with detailed documentation
- `test/TotM2/MovementSpec.hs`: Example of game mechanics testing

## External Dependencies
- **Haskell**: `psqueues` for priority queues, `array` for mutable arrays, `hashable` for state deduplication

When working on this project, understand that performance is critical - the solver handles complex multi-gem puzzles efficiently through bit packing and optimized data structures.
