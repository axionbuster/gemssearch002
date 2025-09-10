# TotM Bug Fix Session Notes

NOTES: These are thoughts written by Claude for Claude and it may not contain accurate information.

## Session Date: September 9, 2025

## Primary Issue
The TotM+ Gem Seeker puzzle solver has a bug where "step 3 is incorrect because a bat (%) collides into the target (*)" but the game continues instead of ending with Lost status.

## Root Cause Analysis
The issue was identified as a **coordinate processing order problem** during gravity application:
- The `range` function processes coordinates in row-major order (left-to-right, top-to-bottom)
- But for `Up` gravity, objects need to be processed **bottom-to-top** to avoid conflicts
- When a bat moves up toward the target, it may not be detected properly if processed in the wrong order

^ HUMAN COMMENT: No, that's not the issue at all, since we are using chaining, a depth-first-traversal approach that is order-independent (it implicitly topologically sorts the pieces). The above list is kept because that is what AI thinks and these notes just describe what it was thinking.

## Solution Strategy
Started implementing an **exception-based control flow** approach instead of post-validation:

1. **New API**: `stepGame :: Direction -> GameState -> Either GameException GameState`
2. **Game Exceptions**: `BatHitTarget | AllGemsCollected` 
3. **Coordinate Ordering**: `orderCoordsForGravity` function to process coordinates in correct order for each direction
4. **Exception Handling**: `chainWithExceptions` that immediately detects bat-target collisions

## Implementation Status

### ✅ COMPLETED
- Added `GameException` data type with `BatHitTarget` and `AllGemsCollected`
- Implemented new `stepGame` API returning `Either GameException GameState`
- Added `orderCoordsForGravity` function for proper coordinate processing
- Implemented `chainWithExceptions` for immediate collision detection
- Added `Outcome(..)` export for backward compatibility with Main.hs
- Resolved Direction constructor naming conflicts by prefixing with `Dir`

### 🔄 IN PROGRESS
- **STUCK ON COMPILATION ERRORS**: The file appears to have multiple conflicting definitions causing ambiguous occurrences

### ❌ BLOCKING ISSUES

#### Current Compilation Errors
The compiler reports:
1. **Ambiguous `Left`/`Right`**: Claims there are both `Data.Either.Left` and `TotM.Left` definitions
2. **Missing `E.Either`**: References to qualified `E.Either` that don't exist in visible code
3. **Line number mismatches**: Error locations don't match visible file content

#### Possible Causes
1. **File state corruption**: Multiple edits may have created invisible duplicates
2. **Compiler cache issues**: `.stack-work` directory may have stale compilation artifacts  
3. **Editor/file sync issues**: VS Code might be showing different content than what compiler sees

## Code Structure (As Should Be)

### Direction Definition
```haskell
data Direction = DirUp | DirDown | DirLeft | DirRight deriving (Eq, Show, Enum, Bounded)
```

### Key Functions Implemented
- `stepGameST :: Direction -> (Int, Int) -> TotM -> ST s (Either GameException TotM)`
- `applyGravityWithExceptions :: Direction -> (Int, Int) -> TotS s -> ST s (Either GameException ())`
- `chainWithExceptions :: ((Int, Int) -> (Int, Int)) -> (Int, Int) -> (Int, Int) -> TotS s -> ST s (Either GameException ())`
- `orderCoordsForGravity :: Direction -> [(Int, Int)] -> [(Int, Int)]`

## Test Case
**Case 2025-37**: The failing scenario where bat collision should end game but doesn't.

## Next Session Actions

### IMMEDIATE (High Priority)
1. **Clean build**: `rm -rf .stack-work; stack build` to clear compiler cache
2. **File verification**: Check if TotM.hs content matches what compiler sees
3. **Line-by-line debugging**: If errors persist, manually verify each error location

### COMPILATION FIXES NEEDED
If errors persist after clean build:
1. **Remove duplicate definitions**: Search for any hidden Direction definitions with old constructors
2. **Fix qualified imports**: Replace any remaining `E.Either` with plain `Either`
3. **Resolve ambiguities**: Ensure no naming conflicts between Direction and Either constructors

### TESTING AFTER COMPILATION
1. **Update test files**: Continue fixing Direction constructor references in test/*.hs files
2. **Add test case**: Create test for case 2025-37 scenario
3. **Coordinate ordering**: Test that `orderCoordsForGravity` fixes the bat movement bug

## Key Files
- `src/TotM.hs` - Main module (currently broken compilation)
- `test/PhysicsSpec.hs` - Needs Direction constructor updates 
- `app/Main.hs` - Needs Outcome(..) export (should be working now)
- `cases/2025-37.in` & `cases/2025-37.out` - Test case files

## Commands to Use
**IMPORTANT**: Use direct terminal commands instead of VS Code tasks due to VS Code bug:
```bash
stack clean
stack build  # NOT via task
stack test   # After fixing compilation
```

## Technical Context
- **Language**: Haskell with Stack build system
- **Key Dependencies**: ST monad, STUArray, Control.Monad.Catch
- **Architecture**: Exception-based control flow for game state management
- **Algorithm**: Dijkstra pathfinding with physics simulation

## Expected Outcome
Once compilation is fixed, the new exception-based approach should:
1. Immediately detect when a bat hits the target during movement
2. Process coordinates in the correct order for each gravity direction  
3. Return `Left BatHitTarget` instead of continuing the game
4. Pass the test case for scenario 2025-37

---
*Session ended due to compilation issues. Next session should focus on clearing build cache and resolving file state conflicts.*
