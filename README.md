# Gem Seeker Solver

A Haskell solver for the Gem Seeker minigame from Tomb of the Mask+.

## Problem Description

You are given a 2D grid containing gems that need to be collected. The goal is to collect all gems by manipulating gravity to slide objects around the grid.

### Game Mechanics

- **Grid Elements:**
  - `.` - Air (empty space)
  - `@` - Gem (movable, must be collected)
  - `%` - Bat (movable, dangerous)
  - `#` - Obstacle/Wall (immovable)
  - `*` - Target (collection point, always appears as air)

- **Physics:**
  - Each move changes gravity to one of four directions: Up, Down, Left, Right
  - All movable objects (gems and bats) slide in the gravity direction until they hit an obstacle, boundary, or another object
  - When a gem slides into the target position, it disappears immediately (gets collected)
  - Objects cannot overlap or teleport through each other

- **Win/Lose Conditions:**
  - **Win:** All gems have been collected (reached the target)
  - **Lose:** A bat reaches the target position
  - **Continue:** Gems remain and no bat is at the target

### Important Invariants

- The target position (`*`) must always contain air in any valid game state
- No gem, bat, or obstacle should ever occupy the target position
- Gems disappear immediately upon sliding into the target

## Input Format

**Note:** Input and output formats are fluid and may change anytime.

```
<number_of_test_cases>
<width> <height>
<grid_row_1>
<grid_row_2>
...
<grid_row_height>
```

### Example Input

```
1
6 8
@...#@
.#....
......
...#..
#..@.#
......
#*....
..#...
```

This represents:
- 1 test case
- Grid dimensions: 6 columns × 8 rows  
- 3 gems at positions: (0,0), (0,5), and (4,3)
- Target at position (6,1)
- Various obstacles (`#`) throughout the grid

## Output Format

For each test case:
- `yes` if a solution exists, `no` if impossible
- If solvable, show the initial state and each step with gravity direction
- Display the final "Won!" or "Lost!" message

Note: the output format is fluid and may change anytime as long as it remains human-readable.

### Example Output

```
Test case 1:
yes
Initial state:
@...#@
.#....
......
...#..
#..@.#
......
#*....
..#...

Step 1: Apply gravity Down
....#.
.#....
......
@..#.@
#....#
......
#*....
..#@..

Step 2: Apply gravity Right
(...)
Won!
```

## Algorithm

The solver uses uniform-cost search (UCS) via Dijkstra's algorithm to find the optimal sequence of gravity changes:

1. **State Representation:** Each game state consists of the board configuration and target position
2. **Search Space:** Each move transitions to a new state by applying gravity in one direction  
3. **Pathfinding:** Explores all possible sequences of moves to find the shortest solution
4. **Optimality:** Guarantees the minimum number of moves required

## Building and Running

```bash
# Build the project
stack build

# Run on input file
stack exec gemssearch002-exe < input.txt

# Run tests
stack test
```

## Implementation Details

- **Language:** Haskell, but with efficient mutable arrays
- **Core Module:** `TotM.hs` contains the game logic and solver
- **Performance:** Handles complex puzzles with multiple gems efficiently

## Example Usage

```bash
echo "1
6 8
@...#@
.#....
......
...#..
#..@.#
......
#*....
..#..." | stack exec gemssearch002-exe
```

## Web Port

A TypeScript/JavaScript web version is available in the `web-port/` directory:

```bash
cd web-port
npm install
npm run build
npm run serve
```

This provides:
- Interactive pairing heap demonstration
- Efficient priority queue implementation for web browsers
- Foundation for a client-side web version of the game solver

See `web-port/README.md` for detailed web development instructions.

## License

BSD-3-Clause
