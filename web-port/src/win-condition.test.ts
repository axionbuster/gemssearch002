/**
 * @fileoverview Win condition tests ported from WinConditionSpec.hs
 * Tests the game outcome logic
 */

import {
 TotM, Air, Bat, Gem, Obs, createBoard, Position,
 checkOutcome, Outcome
} from './totm';

describe('Win condition logic', () => {
 test('should win when no gems left', () => {
  // Create a simple 2x2 board with no gems (all collected)
  const cells = [[Air, Air], [Air, Air]];
  const target: Position = [1, 1];
  const [board] = createBoard(cells, target);
  const outcome = checkOutcome(target, board);

  expect(outcome).toBe(Outcome.Won);
 });

 test('should lose when bat is at target', () => {
  // Create a simple 2x2 board with bat at target
  const cells = [[Air, Air], [Air, Bat]];
  const target: Position = [1, 1];
  const [board] = createBoard(cells, target);
  // Manually place bat at target (this should never happen in normal gameplay)
  board.setCell(target, Bat);
  const outcome = checkOutcome(target, board);

  expect(outcome).toBe(Outcome.Lost);
 });

 test('should be running when gems remain and target is empty', () => {
  // Create a board with gems but target empty (as it should always be)
  const cells = [[Gem, Air], [Air, Air]];
  const target: Position = [1, 1];
  const [board] = createBoard(cells, target);
  const outcome = checkOutcome(target, board);

  expect(outcome).toBe(Outcome.Running);
 });

 test('should be running when multiple gems remain', () => {
  // Create a board with multiple gems
  const cells = [[Gem, Gem], [Gem, Air]];
  const target: Position = [1, 1];
  const [board] = createBoard(cells, target);
  const outcome = checkOutcome(target, board);

  expect(outcome).toBe(Outcome.Running);
 });

 test('should win when last gem is collected', () => {
  // Start with one gem, then collect it
  const cells = [[Gem, Air]];
  const target: Position = [0, 1];
  const [board] = createBoard(cells, target);

  // Before collection - should be running
  expect(checkOutcome(target, board)).toBe(Outcome.Running);

  // Simulate collecting the gem (remove it)
  board.setCell([0, 0], Air);

  // After collection - should win
  expect(checkOutcome(target, board)).toBe(Outcome.Won);
 });

 test('should handle board with obstacles and no gems', () => {
  // Board with obstacles but no gems or bats
  const cells = [[Obs, Air], [Air, Obs]];
  const target: Position = [0, 1];
  const [board] = createBoard(cells, target);
  const outcome = checkOutcome(target, board);

  expect(outcome).toBe(Outcome.Won);
 });

 test('should be running when bat is present but not at target', () => {
  // Board with bat somewhere else and gems still present
  const cells = [[Gem, Bat], [Air, Air]];
  const target: Position = [1, 1];
  const [board] = createBoard(cells, target);
  const outcome = checkOutcome(target, board);

  expect(outcome).toBe(Outcome.Running);
 });

 test('should maintain target invariant - target should always be Air', () => {
  // Test that target position is always Air in valid game states
  const cells = [[Gem, Air], [Air, Air]];
  const target: Position = [1, 1];
  const [board] = createBoard(cells, target);

  // Target should be Air
  expect(board.getCell(target)).toBe(Air);

  // This should hold for all valid game states
  const outcome = checkOutcome(target, board);
  expect(board.getCell(target)).toBe(Air);
 });
});
