/**
 * @fileoverview Timeout and performance tests
 * Tests solver behavior under time constraints and edge cases
 */

import {
 TotM, Air, Bat, Gem, Obs, createBoard, Position,
 GameState, solve, Direction
} from './totm';

describe('Timeout and performance tests', () => {
 test('should timeout on very large unsolvable board', () => {
  // Create a large board that's impossible to solve
  const size = 8;
  const cells: any[][] = [];
  for (let r = 0; r < size; r++) {
   cells[r] = [];
   for (let c = 0; c < size; c++) {
    if (r === 0 && c === 0) {
     cells[r][c] = Gem; // Single gem at top-left
    } else if (r === size - 1 && c === size - 1) {
     cells[r][c] = Air; // Target at bottom-right
    } else {
     cells[r][c] = Obs; // Everything else is obstacles
    }
   }
  }

  const target: Position = [size - 1, size - 1];
  const [board] = createBoard(cells, target);
  const gameState = new GameState(board, target);

  // This should timeout or return null quickly
  const result = solve(gameState);
  expect(result).toBeNull(); // Should be impossible to solve
 }, 2000); // 2 second timeout - should fail fast

 test('should solve simple cases quickly', () => {
  const startTime = Date.now();

  // Simple case that should solve in milliseconds
  const cells = [[Gem, Air]];
  const target: Position = [0, 1];
  const [board] = createBoard(cells, target);
  const gameState = new GameState(board, target);
  const result = solve(gameState);

  const elapsed = Date.now() - startTime;

  expect(result).not.toBeNull();
  expect(elapsed).toBeLessThan(100); // Should solve in under 100ms
  if (result) {
   expect(result.length).toBe(1);
   expect(result[0]).toBe(Direction.Right);
  }
 }, 1000); // 1 second timeout

 test('should handle moderately complex case within timeout', () => {
  // A case that requires some search but shouldn't timeout
  const cells = [
   [Gem, Air, Air, Air],
   [Obs, Obs, Obs, Air],
   [Air, Air, Air, Air],
   [Air, Air, Air, Air]
  ];
  const target: Position = [3, 3];
  const [board] = createBoard(cells, target);
  const gameState = new GameState(board, target);

  const startTime = Date.now();
  const result = solve(gameState);
  const elapsed = Date.now() - startTime;

  console.log(`Moderate case solved in ${elapsed}ms`);

  // Should find a solution in reasonable time
  expect(result).not.toBeNull();
  expect(elapsed).toBeLessThan(1000); // Should solve in under 1 second
 }, 5000); // 5 second timeout

 test('should handle pathological input without hanging', () => {
  // Create a board with many gems and bats in a complex layout
  const cells = [
   [Gem, Bat, Gem, Bat, Air],
   [Bat, Gem, Bat, Gem, Air],
   [Gem, Bat, Gem, Bat, Air],
   [Bat, Gem, Bat, Gem, Air],
   [Air, Air, Air, Air, Air]
  ];
  const target: Position = [4, 4];
  const [board] = createBoard(cells, target);
  const gameState = new GameState(board, target);

  // This might be solvable or not, but should terminate quickly
  const startTime = Date.now();
  const result = solve(gameState);
  const elapsed = Date.now() - startTime;

  console.log(`Pathological case completed in ${elapsed}ms, result: ${result ? 'solved' : 'no solution'}`);

  // Main requirement: should not hang indefinitely
  expect(elapsed).toBeLessThan(3000); // Should complete in under 3 seconds
 }, 5000); // 5 second timeout

 test('should terminate search when no solution exists', () => {
  // Board where gem is completely isolated
  const cells = [
   [Obs, Obs, Obs],
   [Obs, Gem, Obs],
   [Obs, Obs, Air]
  ];
  const target: Position = [2, 2];
  const [board] = createBoard(cells, target);
  const gameState = new GameState(board, target);

  const startTime = Date.now();
  const result = solve(gameState);
  const elapsed = Date.now() - startTime;

  expect(result).toBeNull(); // Should be impossible
  expect(elapsed).toBeLessThan(500); // Should fail fast
 }, 2000); // 2 second timeout

 test('should handle empty board (immediate win)', () => {
  // Board with no gems - should win immediately
  const cells = [[Air, Air], [Air, Air]];
  const target: Position = [1, 1];
  const [board] = createBoard(cells, target);
  const gameState = new GameState(board, target);

  const startTime = Date.now();
  const result = solve(gameState);
  const elapsed = Date.now() - startTime;

  expect(result).not.toBeNull();
  expect(elapsed).toBeLessThan(10); // Should be nearly instant
  if (result) {
   expect(result.length).toBe(0); // No moves needed
  }
 }, 1000); // 1 second timeout

 test('should respect Jest timeout on hanging solver', async () => {
  // This test ensures that Jest's timeout mechanism works
  // We'll create a scenario that might cause infinite loops

  return new Promise((resolve, reject) => {
   // Create a timer to ensure the test doesn't hang Jest
   const timer = setTimeout(() => {
    reject(new Error('Test took too long - Jest timeout should have fired first'));
   }, 1500);

   try {
    // Create a moderately complex case
    const cells = [
     [Gem, Air, Air, Gem],
     [Air, Obs, Obs, Air],
     [Air, Obs, Obs, Air],
     [Gem, Air, Air, Air]
    ];
    const target: Position = [3, 3];
    const [board] = createBoard(cells, target);
    const gameState = new GameState(board, target);

    const result = solve(gameState);

    clearTimeout(timer);

    // If we get here, the solver completed
    console.log('Solver completed normally');
    resolve(result);

   } catch (error) {
    clearTimeout(timer);
    reject(error);
   }
  });
 }, 1000); // 1 second Jest timeout - should be hit if solver hangs
});
