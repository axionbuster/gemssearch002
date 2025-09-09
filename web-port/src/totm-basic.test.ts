/**
 * @fileoverview Basic TotM data structure tests
 * Tests the optimized flat byte array implementation
 */

import {
 TotM, Air, Bat, Gem, Obs, createBoard, showBoard, Position,
 isAir
} from './totm';

describe('TotM data structure', () => {
 test('should create board and store cells correctly', () => {
  const cells = [
   [Air, Gem, Air],
   [Bat, Obs, Air],
   [Air, Air, Air]
  ];
  const target: Position = [2, 2];
  const [board] = createBoard(cells, target);

  // Verify all cells are stored correctly
  expect(board.getCell([0, 0])).toBe(Air);
  expect(board.getCell([0, 1])).toBe(Gem);
  expect(board.getCell([0, 2])).toBe(Air);
  expect(board.getCell([1, 0])).toBe(Bat);
  expect(board.getCell([1, 1])).toBe(Obs);
  expect(board.getCell([1, 2])).toBe(Air);
  expect(board.getCell([2, 0])).toBe(Air);
  expect(board.getCell([2, 1])).toBe(Air);
  expect(board.getCell([2, 2])).toBe(Air);
 });

 test('should handle bounds checking correctly', () => {
  const cells = [[Air, Air], [Air, Air]];
  const target: Position = [0, 0];
  const [board] = createBoard(cells, target);

  // In bounds
  expect(board.inBounds([0, 0])).toBe(true);
  expect(board.inBounds([0, 1])).toBe(true);
  expect(board.inBounds([1, 0])).toBe(true);
  expect(board.inBounds([1, 1])).toBe(true);

  // Out of bounds
  expect(board.inBounds([-1, 0])).toBe(false);
  expect(board.inBounds([0, -1])).toBe(false);
  expect(board.inBounds([2, 0])).toBe(false);
  expect(board.inBounds([0, 2])).toBe(false);
  expect(board.inBounds([2, 2])).toBe(false);
 });

 test('should clone board correctly', () => {
  const cells = [
   [Gem, Bat],
   [Obs, Air]
  ];
  const target: Position = [1, 1];
  const [board] = createBoard(cells, target);
  const cloned = board.clone();

  // Should be equal but different objects
  expect(board.equals(cloned)).toBe(true);
  expect(board).not.toBe(cloned);

  // Modifying clone shouldn't affect original
  cloned.setCell([0, 0], Air);
  expect(board.equals(cloned)).toBe(false);
  expect(board.getCell([0, 0])).toBe(Gem);
  expect(cloned.getCell([0, 0])).toBe(Air);
 });

 test('should handle memory-efficient storage', () => {
  // Create a larger board to test memory efficiency
  const largeBoard = Array(10).fill(null).map(() =>
   Array(10).fill(Air)
  );
  const [board] = createBoard(largeBoard, [0, 0]);

  const memoryUsage = board.getMemoryUsage();
  const expectedBytes = Math.ceil((10 * 10) / 4); // 4 cells per byte
  expect(memoryUsage).toBe(expectedBytes);
 });

 test('should set and get all cell types correctly', () => {
  const cells = [[Air]];
  const target: Position = [0, 0];
  const [board] = createBoard(cells, target);

  // Test setting each cell type
  board.setCell([0, 0], Air);
  expect(board.getCell([0, 0])).toBe(Air);

  board.setCell([0, 0], Bat);
  expect(board.getCell([0, 0])).toBe(Bat);

  board.setCell([0, 0], Gem);
  expect(board.getCell([0, 0])).toBe(Gem);

  board.setCell([0, 0], Obs);
  expect(board.getCell([0, 0])).toBe(Obs);
 });

 test('should display board correctly', () => {
  const cells = [
   [Air, Gem, Air],
   [Bat, Obs, Air],
   [Air, Air, Air]
  ];
  const target: Position = [2, 2];
  const [board] = createBoard(cells, target);

  const display = showBoard(board, target);
  const expected = ".@.\\n%#.\\n..*";
  expect(display).toBe(expected);
 });

 test('should handle edge cases in bit packing', () => {
  // Test all 4 cell types in a single row (tests byte boundary)
  const cells = [[Air, Bat, Gem, Obs]];
  const target: Position = [0, 0];
  const [board] = createBoard(cells, target);

  expect(board.getCell([0, 0])).toBe(Air);
  expect(board.getCell([0, 1])).toBe(Bat);
  expect(board.getCell([0, 2])).toBe(Gem);
  expect(board.getCell([0, 3])).toBe(Obs);

  // Modify each and verify
  board.setCell([0, 0], Obs);
  board.setCell([0, 1], Gem);
  board.setCell([0, 2], Bat);
  board.setCell([0, 3], Air);

  expect(board.getCell([0, 0])).toBe(Obs);
  expect(board.getCell([0, 1])).toBe(Gem);
  expect(board.getCell([0, 2])).toBe(Bat);
  expect(board.getCell([0, 3])).toBe(Air);
 });

 test('should handle single cell board', () => {
  const cells = [[Air]];
  const target: Position = [0, 0];
  const [board] = createBoard(cells, target);

  expect(board.getCell([0, 0])).toBe(Air);
  expect(board.inBounds([0, 0])).toBe(true);
  expect(board.inBounds([0, 1])).toBe(false);
  expect(board.inBounds([1, 0])).toBe(false);
 });

 test('should provide consistent hash codes', () => {
  const cells = [[Gem, Air], [Air, Bat]];
  const target: Position = [1, 1];
  const [board1] = createBoard(cells, target);
  const [board2] = createBoard(cells, target);

  expect(board1.hashCode()).toBe(board2.hashCode());
  expect(board1.equals(board2)).toBe(true);

  // Different boards should have different hashes (usually)
  board2.setCell([0, 0], Air);
  expect(board1.hashCode()).not.toBe(board2.hashCode());
  expect(board1.equals(board2)).toBe(false);
 });

 test('isAir utility function', () => {
  expect(isAir(Air)).toBe(true);
  expect(isAir(Bat)).toBe(false);
  expect(isAir(Gem)).toBe(false);
  expect(isAir(Obs)).toBe(false);
 });
});
