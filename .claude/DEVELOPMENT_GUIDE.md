# Claude Development Guide - Gem Seeker Solver

**Audience**: Claude AI Assistant (future sessions)  
**Purpose**: Critical information to avoid common mistakes and maintain project consistency

This guide contains essential context for working on the Gem Seeker solver project. The user has specifically requested that Claude assistants read this carefully to avoid repeated mistakes and maintain coding standards.

## 🐚 Shell Environment: Context-Dependent

**Important Shell Context:**

- **Claude Code (VS Code/Project Goose/etc.)**: Always uses bash emulation, even on Windows
- **Claude in other clients**: Must use the user's actual shell (Nushell in this case)
- **This project**: User has Nushell installed, so use Nushell syntax when available

### ❌ WRONG (bash syntax in Nushell environment):
```bash
stack exec gemssearch002-exe < input.txt
```
**Problem**: In Nushell, `<` is NOT a redirection operator (*in this context*). It gets passed as a literal argument to the program, making it appear the program is hanging when it's actually waiting for the literal string `<` as input.

### ✅ CORRECT (Nushell syntax):
```nu
open --raw input.txt | stack exec gemssearch002-exe
```
**How it works**: `open --raw` reads the file content and pipes it to the program's stdin.

### Common Nushell Commands:
- Pipe files: `open --raw file.txt | command`
- List files: `ls`
- Change directory: `cd path`
- Check command existence: `which command-name`
- Get command output: Command output automatically captured
- Fallback: If Nushell unavailable, use standard shell syntax

## 🎮 Game Rules - MUST READ CAREFULLY

The Gem Seeker game has very specific rules that are easy to misunderstand:

### Core Elements:
- `@` = Gem (movable, must be collected)
- `%` = Bat (movable, dangerous)
- `#` = Obstacle/Wall (immovable)
- `.` = Air (empty space)
- `*` = Target (collection point)

### 🔥 CRITICAL INVARIANT:
**The target position (`*`) MUST ALWAYS contain Air in any valid game state.**
- NO gem should ever be found at the target position
- NO bat should ever be at the target position  
- NO obstacle should ever be at the target position
- The target is a "sink" where gems disappear upon contact
- This condition holds for the starting state, any intermediary state in the solution path, and the final state.

### Physics Rules:
1. **Gravity Application**: Each move changes gravity to Up/Down/Left/Right
2. **Object Movement**: All movable objects (gems & bats) slide until they hit:
   - An obstacle (`#`)
   - The grid boundary
   - Another movable object
3. **Gem Collection**: When a gem slides INTO the target, it disappears immediately
4. **Chain Reactions**: Objects can push other objects in sequence

### Win/Lose Conditions:
- **WIN**: All gems collected (gemCount == 0)
- **LOSE**: A bat reaches the target position
- **CONTINUE**: Gems remain and no bat at target

### Example Scenario:
```
Initial: [., @, @, ., *, #]  (2 gems, target at position 4)
Apply gravity RIGHT:
1. First gem slides right, hits second gem
2. Second gem slides right, hits target and DISAPPEARS
3. First gem slides into the space left by second gem
4. First gem continues sliding and hits target, DISAPPEARS
Result: [., ., ., ., *, #]   (0 gems left = WIN in ONE move!)
```

### What to Do When Tests Are Incorrect:

Sometimes, the tests reflect a misunderstanding about the game rules. When that happens, this document should take precedence. Consult with the user.

## 💻 Coding Style (follow src/Dijk.hs)

### Documentation Style:
```haskell
{- |
Module      : ModuleName
Description : Brief description
Copyright   : (c) 2025 axionbuster
License     : BSD-3-Clause
Maintainer  : axionbuster

Detailed module description with examples.

Key features:
* __Feature 1__: Description
* __Feature 2__: Description

== Example Usage
@
example code here
@
-}
```

### Function Documentation:
```haskell
{- |
Brief function description.

Detailed explanation with important notes.

IMPORTANT: Critical constraints or invariants.

Returns:
- Case 1: description
- Case 2: description
-}
functionName :: Type -> Type
```

### Code Formatting:
- **Alignment**: Use consistent spacing for readability
- **Imports**: Rely on `stylish-haskell`, or imitate what it would do
- **Type Signatures**: Always explicit, well-documented
- **Comments**: Explain WHY, not just WHAT

### Naming Conventions:
- **Local identifiers**: Prefer concise names (`r`, `c`, `ij`, `cell`, `dir`) over verbose ones
- **Concise naming creates visual hierarchy**: Important/tricky concepts get longer names, but verbose names are only sometimes allowed to make a rhetorical point, draw attention to a particular variable, create contrast, etc.
- **Exported functions/types**: Follow standard Haskell conventions (descriptive names)
- **Examples**:
  - ✅ Local: `r`, `c`, `dir`, `cell`, `outcome`  
  - ✅ Exported: `checkOutcome`, `GameState`, `applyGravity`
  - ⚖️ Local: `currentRowIndex`, `currentColumnIndex`, `directionToApply`

The hierarchy of information concept means that when scanning code, your eye should immediately notice the important parts. Most variables use short, clear names that fade into the background (`r`, `c`, `cell`), while occasionally a longer name (`initialGameState`, `criticalBoundaryCondition`) jumps out to signal "pay attention here." This creates a natural reading rhythm where routine operations are quiet and important concepts are loud.

### Common Patterns:
```haskell
-- Pattern matching with guards
func input = case input of
  Pattern1 | condition -> result1
  Pattern2             -> result2
  _                    -> defaultResult

-- ST monad usage for mutable arrays
operation :: ST s (Result)
operation = do
  mutableArray <- thaw immutableArray
  -- mutations here
  result <- freeze mutableArray
  pure result
```

## 🎨 Formatting Instructions

### Primary: VS Code Haskell Extension
1. Try VS Code's format command if available
2. The project has `haskell.haskell` extension installed

### Fallback: Manual stylish-haskell
```nu
# Check if available
which stylish-haskell

# If available, format all Haskell files
ls **/*.hs | each { |file| stylish-haskell -i $file.name }
```

### Last Resort: Manual Formatting
- Follow existing code style in src/Dijk.hs
- Consistent indentation (spaces, not tabs)
- Align similar constructs vertically
- Keep line length reasonable (80 chars in Haskell files)

## 🧪 Testing Strategy

### Test Categories:
1. **Integration Tests**: End-to-end solver functionality (KEEP THESE)
2. **Physics Tests**: Game mechanics and movement (MAY NEED FIXES)
3. **Win Condition Tests**: Outcome logic (MAY NEED FIXES)
4. **Debug Tests**: Development helpers (CAN BE MODIFIED/DELETED)

### ⚠️ Test Fixing Rules:
- **NEVER** expect gems to remain at target position
- Tests expecting `Gem` at target are categorically wrong
- If gems slide into target, they disappear (target becomes `Air`)
- Integration tests are the gold standard - if they pass, core logic is correct

### Common Test Fixes:
```haskell
-- ❌ WRONG: expecting gem at target
totMIndex board target `shouldBe` Gem

-- ✅ CORRECT: gem collected, target remains Air  
totMIndex board target `shouldBe` Air
```

## 📁 Project Structure

```
├── src/
│   ├── Dijk.hs          # Dijkstra algorithm (DON'T MODIFY)
│   ├── TotM.hs          # Core game logic (MAIN WORK HERE)
│   └── Lib.hs           # Utility functions
├── app/
│   └── Main.hs          # CLI interface
├── test/
│   ├── IntegrationSpec.hs    # END-TO-END TESTS (CRITICAL)
│   ├── PhysicsSpec.hs        # Game mechanics tests
│   ├── WinConditionSpec.hs   # Outcome logic tests
│   └── DebugSpec.hs          # Development helpers
├── case0.txt            # Example input
├── case0_out0.txt       # Example output
└── README.md            # User documentation
```

## 🚨 Common Claude Mistakes to Avoid

**These are patterns Claude frequently gets wrong:**

1. **Shell Syntax Context**: 
   - Claude Code: Use bash syntax (always)
   - Other Claude clients: Use user's actual shell (Nushell here)
   - Don't assume bash `<` redirection works in Nushell
2. **Target Invariant**: Never put anything except Air at target position
3. **Gem Physics**: Gems disappear when sliding INTO target, not when AT target  
4. **Test Expectations**: Don't expect gems to remain at target position
5. **Win Condition**: Win only when ALL gems collected, not just one
6. **Code Style**: Follow Dijk.hs patterns for consistency

## 🎯 Claude Success Criteria & Commands

**Always use Nushell syntax for terminal commands!**

```nu
# Build project
stack build

# Run tests  
stack test

# Run on input file
open --raw input.txt | stack exec gemssearch002-exe

# Run specific test
stack test --test-arguments "--match \"test name\""

# Check git status
git status

# View changes
git diff
```

## 📝 Key Files to Remember

- **TotM.hs**: Main game logic, solver, and physics
- **case0.txt**: Reference input with 3 gems
- **case0_out0.txt**: Expected output showing all 3 gems collected
- **README.md**: User-facing documentation
- **This file**: Development instructions

## 🎯 Success Criteria

A working solution should:
1. Build without errors (`stack build`)
2. Pass integration tests (`stack test --test-arguments "--match Integration"`)
3. Solve case0 by collecting all 3 gems
4. Use consistent input/output format (`@` for gems, `*` for target)
5. Maintain code style consistent with Dijk.hs

Remember: Integration tests passing = core functionality works correctly!
