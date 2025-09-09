/**
 * @fileoverview TypeScript port of TotM.hs - Tomb of the Mask+ Gem Seeker solver
 * @author axionbuster
 * @license BSD-3-Clause
 * 
 * Solves the Gem Seeker minigame from Tomb of the Mask+ using efficient
 * pathfinding algorithms. This is a direct port of the Haskell TotM module.
 */

import { dijk, recon } from './dijk';

// ===== CORE TYPES =====

/**
 * Cell type representing the contents of a board position.
 * Uses the same bit patterns as the Haskell version.
 */
export type Cell = number; // Word8 equivalent, only low 2 bits are valid

export const Air: Cell = 0b00;
export const Bat: Cell = 0b01;
export const Gem: Cell = 0b10;
export const Obs: Cell = 0b11; // Obstacle/Wall

export function isAir(cell: Cell): boolean {
 return cell === Air;
}

/**
 * Game outcome states
 */
export enum Outcome {
 Running = 'Running',
 Won = 'Won',
 Lost = 'Lost'
}

/**
 * Gravity directions - exactly 4 possibilities
 */
export enum Direction {
 Up = 'Up',
 Down = 'Down',
 Left = 'Left',
 Right = 'Right'
}

/**
 * Position on the board [row, col]
 */
export type Position = [number, number];

/**
 * Board bounds [[minRow, minCol], [maxRow, maxCol]]
 */
export type Bounds = [Position, Position];

/**
 * Game board - flat byte array with 2 bits per cell
 * Equivalent to Haskell's TotM newtype wrapping UArray
 * 
 * Memory layout: Each byte stores 4 cells (2 bits each)
 * Performance: ~4x memory reduction, better cache locality, faster copying
 */
export class TotM {
 private readonly data: Uint8Array;
 private readonly rows: number;
 private readonly cols: number;
 private readonly bounds: Bounds;

 constructor(board: Cell[][] | Uint8Array, rows?: number, cols?: number) {
  if (board instanceof Uint8Array) {
   // Direct construction from flat array
   this.data = new Uint8Array(board);
   this.rows = rows!;
   this.cols = cols!;
  } else {
   // Construction from 2D array
   this.rows = board.length;
   this.cols = this.rows > 0 ? board[0].length : 0;

   const totalCells = this.rows * this.cols;
   const bytesNeeded = Math.ceil(totalCells / 4); // 4 cells per byte
   this.data = new Uint8Array(bytesNeeded);

   // Pack 2D array into flat bytes
   for (let r = 0; r < this.rows; r++) {
    for (let c = 0; c < this.cols; c++) {
     this.setCell([r, c], board[r][c]);
    }
   }
  }

  this.bounds = [[0, 0], [this.rows - 1, this.cols - 1]];
 }

 /**
  * Convert 2D position to flat index
  */
 private positionToIndex(pos: Position): number {
  const [r, c] = pos;
  return r * this.cols + c;
 }

 /**
  * Get the byte index and bit offset for a cell
  */
 private getCellLocation(pos: Position): [number, number] {
  const flatIndex = this.positionToIndex(pos);
  const byteIndex = Math.floor(flatIndex / 4);
  const bitOffset = (flatIndex % 4) * 2; // 2 bits per cell
  return [byteIndex, bitOffset];
 }

 /**
  * Get board bounds
  */
 getBounds(): Bounds {
  return this.bounds;
 }

 /**
  * Get cell at position
  */
 getCell(pos: Position): Cell {
  const [r, c] = pos;
  if (!this.inBounds(pos)) {
   throw new Error(`Position [${r}, ${c}] out of bounds`);
  }

  const [byteIndex, bitOffset] = this.getCellLocation(pos);
  const byte = this.data[byteIndex];
  const mask = 0b11 << bitOffset; // 2-bit mask
  return (byte & mask) >> bitOffset;
 }

 /**
  * Set cell at position
  */
 setCell(pos: Position, cell: Cell): void {
  const [r, c] = pos;
  if (!this.inBounds(pos)) {
   throw new Error(`Position [${r}, ${c}] out of bounds`);
  }

  const [byteIndex, bitOffset] = this.getCellLocation(pos);
  const mask = 0b11 << bitOffset; // 2-bit mask
  const clearedByte = this.data[byteIndex] & ~mask; // Clear the 2 bits
  const newByte = clearedByte | ((cell & 0b11) << bitOffset); // Set new value
  this.data[byteIndex] = newByte;
 }

 /**
  * Check if position is within bounds
  */
 inBounds(pos: Position): boolean {
  const [r, c] = pos;
  return r >= 0 && r < this.rows && c >= 0 && c < this.cols;
 }

 /**
  * Create a copy of the board (fast byte array copy)
  */
 clone(): TotM {
  return new TotM(new Uint8Array(this.data), this.rows, this.cols);
 }

 /**
  * Get all positions on the board
  */
 getAllPositions(): Position[] {
  const positions: Position[] = [];
  for (let r = 0; r < this.rows; r++) {
   for (let c = 0; c < this.cols; c++) {
    positions.push([r, c]);
   }
  }
  return positions;
 }

 /**
  * Equality check for boards (fast byte array comparison)
  */
 equals(other: TotM): boolean {
  if (this.rows !== other.rows || this.cols !== other.cols) {
   return false;
  }

  if (this.data.length !== other.data.length) {
   return false;
  }

  for (let i = 0; i < this.data.length; i++) {
   if (this.data[i] !== other.data[i]) {
    return false;
   }
  }

  return true;
 }

 /**
  * Hash code for use in Maps (fast byte array hash)
  */
 hashCode(): string {
  // Fast hash using byte array content
  let hash = '';
  for (let i = 0; i < this.data.length; i++) {
   hash += this.data[i].toString(16).padStart(2, '0');
  }
  return `${this.rows}x${this.cols}:${hash}`;
 }

 /**
  * Get raw byte data (for debugging/serialization)
  */
 getRawData(): Uint8Array {
  return new Uint8Array(this.data);
 }

 /**
  * Get memory usage in bytes
  */
 getMemoryUsage(): number {
  return this.data.length;
 }
}

// ===== GAME MECHANICS =====

/**
 * Get direction vector for movement
 */
function directionVector(dir: Direction): Position {
 switch (dir) {
  case Direction.Up: return [-1, 0];
  case Direction.Down: return [1, 0];
  case Direction.Left: return [0, -1];
  case Direction.Right: return [0, 1];
 }
}

/**
 * Get next coordinate in a direction
 */
function nextCoord(dir: Direction, pos: Position): Position {
 const [r, c] = pos;
 const [dr, dc] = directionVector(dir);
 return [r + dr, c + dc];
}

/**
 * Move a piece forward in a chain reaction.
 * This is the core physics simulation - pieces slide until they hit something.
 */
function chain(
 nextCoordFn: (pos: Position) => Position,
 target: Position,
 startPos: Position,
 game: TotM
): void {
 let currentPos = startPos;

 // Keep moving forward until stopped
 while (game.inBounds(currentPos)) {
  const cell = game.getCell(currentPos);

  if (isAir(cell)) {
   break; // Nothing to move
  }

  const nextPos = nextCoordFn(currentPos);

  if (!game.inBounds(nextPos)) {
   break; // Hit boundary, stop
  }

  const nextCell = game.getCell(nextPos);

  if (nextCell === Air) {
   // Move forward
   game.setCell(currentPos, Air);

   // Special case: gem disappears when hitting target
   if (nextPos[0] === target[0] && nextPos[1] === target[1] && cell === Gem) {
    game.setCell(nextPos, Air);
   } else {
    game.setCell(nextPos, cell);
   }

   currentPos = nextPos;
  } else if (nextCell === Obs) {
   break; // Hit obstacle, stop
  } else {
   // Hit movable object - process it first
   chain(nextCoordFn, target, nextPos, game);

   // Try again - the space might be clear now
   const nextCellAfter = game.getCell(nextPos);
   if (nextCellAfter === Air) {
    game.setCell(currentPos, Air);

    // Special case: gem disappears when hitting target
    if (nextPos[0] === target[0] && nextPos[1] === target[1] && cell === Gem) {
     game.setCell(nextPos, Air);
    } else {
     game.setCell(nextPos, cell);
    }

    currentPos = nextPos;
   } else {
    break; // Still blocked
   }
  }
 }
}

/**
 * Count gems on the board
 */
function countGems(game: TotM): number {
 const positions = game.getAllPositions();
 return positions.filter(pos => game.getCell(pos) === Gem).length;
}

/**
 * Apply gravity in a direction to the entire game.
 * 
 * Moves all movable objects (gems and bats) in the specified direction until
 * they hit obstacles, boundaries, or other objects. Returns the new board
 * state and the game outcome.
 */
export function applyGravity(dir: Direction, target: Position, game: TotM): [TotM, Outcome] {
 const newGame = game.clone();
 const positions = newGame.getAllPositions();

 // Move all movable objects
 for (const pos of positions) {
  const cell = newGame.getCell(pos);
  if (cell === Bat || cell === Gem) {
   chain((p: Position) => nextCoord(dir, p), target, pos, newGame);
  }
 }

 // Check outcome
 const outcome = checkOutcome(target, newGame);
 return [newGame, outcome];
}

/**
 * Check the current game outcome.
 * 
 * IMPORTANT INVARIANT: The target position should always contain Air in any valid 
 * game state. No gem, bat, or obstacle should ever occupy the target position.
 * This invariant is not enforced by this function but is a fundamental requirement
 * of the game mechanics. Violations may lead to undefined behavior.
 * 
 * Returns:
 * - Won: when all gems have been collected (gemCount == 0)  
 * - Lost: when a bat occupies the target position
 * - Running: when the game is still in progress
 */
export function checkOutcome(target: Position, game: TotM): Outcome {
 const targetCell = game.getCell(target);
 const gemCount = countGems(game);

 if (targetCell === Bat) {
  return Outcome.Lost;
 } else if (gemCount === 0) {
  return Outcome.Won; // All gems collected wins!
 } else {
  return Outcome.Running;
 }
}

// ===== GAME STATE AND PATHFINDING =====

/**
 * Game state for pathfinding
 */
export class GameState {
 constructor(
  public readonly board: TotM,
  public readonly target: Position
 ) { }

 /**
  * Equality check for game states
  */
 equals(other: GameState): boolean {
  return this.board.equals(other.board) &&
   this.target[0] === other.target[0] &&
   this.target[1] === other.target[1];
 }

 /**
  * Hash code for use in Maps
  */
 hashCode(): string {
  return `${this.board.hashCode()}:${this.target[0]},${this.target[1]}`;
 }
}

/**
 * Get all possible next states from current state
 */
export function* neighbors(state: GameState): Iterable<[Direction, GameState]> {
 const directions = [Direction.Up, Direction.Down, Direction.Left, Direction.Right];

 for (const dir of directions) {
  const [newBoard, outcome] = applyGravity(dir, state.target, state.board);

  if (outcome !== Outcome.Lost) {
   yield [dir, new GameState(newBoard, state.target)];
  }
 }
}

/**
 * Solve the gem seeker puzzle using uniform-cost search.
 * 
 * Finds the optimal sequence of gravity changes to collect all gems and reach
 * the target, or returns null if no solution exists.
 */
export function solve(startState: GameState): Direction[] | null {
 const isWon = (state: GameState): boolean => {
  const outcome = checkOutcome(state.target, state.board);
  return outcome === Outcome.Won;
 };

 const weight = (from: GameState, to: GameState): number => 1; // Each move costs 1

 // Create a map-friendly version of neighbors
 const neighborsMap = (state: GameState): [Direction, GameState][] => {
  return Array.from(neighbors(state));
 };

 const dijkResult = dijk(weight, neighborsMap, isWon, startState);

 if (!dijkResult.target) {
  return null; // No winning state found
 }

 const [, moveList] = recon(dijkResult, dijkResult.target);

 return moveList.length > 0 ? moveList : null;
}

// ===== BOARD CREATION AND DISPLAY =====

/**
 * Create a game board from a 2D array of cells.
 * 
 * Takes a 2D array of cells and a target position,
 * returns the game board and target position.
 */
export function createBoard(cells: Cell[][], target: Position): [TotM, Position] {
 const board = new TotM(cells);
 return [board, target];
}

/**
 * Create a game board from string representation (like input files).
 * 
 * String format:
 * - '.' = Air
 * - '@' = Gem  
 * - '%' = Bat
 * - '#' = Obstacle
 * - '*' = Target (becomes Air in board, position returned separately)
 * 
 * @param boardString Multi-line string representation
 * @returns [TotM board, target position]
 */
export function createBoardFromString(boardString: string): [TotM, Position] {
 const lines = boardString.trim().split(/\\r?\\n/).filter(line => line.length > 0);
 const cells: Cell[][] = [];
 let target: Position | null = null;

 for (let r = 0; r < lines.length; r++) {
  const line = lines[r].trim(); // Remove any extra whitespace
  const row: Cell[] = [];
  for (let c = 0; c < line.length; c++) {
   const char = line[c];
   let cell: Cell;
   switch (char) {
    case '.': cell = Air; break;
    case '@': cell = Gem; break;
    case '%': cell = Bat; break;
    case '#': cell = Obs; break;
    case '*':
     cell = Air; // Target position contains Air
     target = [r, c];
     break;
    case '\n':
     // Skip newlines
     cell = -1;
     break;
    default:
     throw new Error(`Invalid character '${char}' (code: ${char.charCodeAt(0)}) at position [${r}, ${c}]`);
   }
   if (cell !== -1)
    row.push(cell);
  }

  if (row.length > 0) { // Only add non-empty rows
   cells.push(row);
  }
 }

 if (target === null) {
  throw new Error('No target position (*) found in board string');
 }

 return createBoard(cells, target);
}

/**
 * Pretty print a game board using the same format as input.
 * 
 * Displays the board with '*' for target, '@' for gems, '%' for bats, 
 * '#' for obstacles, and '.' for air.
 */
export function showBoard(board: TotM, target: Position): string {
 const [[r1, c1], [r2, c2]] = board.getBounds();
 const rows: string[] = [];

 for (let r = r1; r <= r2; r++) {
  let row = '';
  for (let c = c1; c <= c2; c++) {
   const pos: Position = [r, c];
   if (pos[0] === target[0] && pos[1] === target[1]) {
    row += '*';
   } else {
    const cell = board.getCell(pos);
    switch (cell) {
     case Air: row += '.'; break;
     case Bat: row += '%'; break;
     case Gem: row += '@'; break;
     case Obs: row += '#'; break;
     default: row += '?'; break;
    }
   }
  }
  rows.push(row);
 }

 return rows.join('\\n');
}
