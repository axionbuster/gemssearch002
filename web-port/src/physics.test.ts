/**
 * @fileoverview Physics tests ported from PhysicsSpec.hs
 * Tests the core gravity mechanics and object movement
 */

import {
 TotM, Air, Bat, Gem, Obs, createBoard, Position, Direction,
 applyGravity, Outcome
} from './totm';

describe('Gravity physics', () => {
 test('should move gem down with Down gravity', () => {
  // Create a 3x3 board with gem at top
  const cells = [[Gem, Air, Air], [Air, Air, Air], [Air, Air, Air]];
  const target: Position = [2, 2];
  const [board] = createBoard(cells, target);
  const [newBoard, outcome] = applyGravity(Direction.Down, target, board);

  // Gem should move to bottom row
  expect(newBoard.getCell([2, 0])).toBe(Gem);
  expect(newBoard.getCell([0, 0])).toBe(Air);
  expect(outcome).toBe(Outcome.Running);
 });

 test('should move gem right with Right gravity', () => {
  // Create a 3x3 board with gem at left
  const cells = [[Gem, Air, Air], [Air, Air, Air], [Air, Air, Air]];
  const target: Position = [2, 2];
  const [board] = createBoard(cells, target);
  const [newBoard, outcome] = applyGravity(Direction.Right, target, board);

  // Gem should move to right column
  expect(newBoard.getCell([0, 2])).toBe(Gem);
  expect(newBoard.getCell([0, 0])).toBe(Air);
  expect(outcome).toBe(Outcome.Running);
 });

 test('should collect gem when it slides into target', () => {
  // Create a board with gem sliding into target position  
  const cells = [[Gem, Air, Obs]];
  const target: Position = [0, 1];
  const [board] = createBoard(cells, target);
  const [newBoard, outcome] = applyGravity(Direction.Right, target, board);

  // Gem should disappear when it reaches target
  expect(newBoard.getCell([0, 1])).toBe(Air); // target remains Air
  expect(newBoard.getCell([0, 0])).toBe(Air); // gem moved from here
  expect(newBoard.getCell([0, 2])).toBe(Obs); // obstacle unchanged
  expect(outcome).toBe(Outcome.Won); // all gems collected
 });

 test('should push gems in chain', () => {
  // Create a board with two gems in a row
  const cells = [[Gem, Gem, Air]];
  const target: Position = [0, 2];
  const [board] = createBoard(cells, target);
  const [newBoard, outcome] = applyGravity(Direction.Right, target, board);

  // Expected behavior: first gem pushes second gem to target (collected),
  // then first gem continues to target and is also collected
  expect(newBoard.getCell([0, 0])).toBe(Air);
  expect(newBoard.getCell([0, 1])).toBe(Air);
  expect(newBoard.getCell([0, 2])).toBe(Air); // target remains Air, both gems were collected
  expect(outcome).toBe(Outcome.Won); // All gems collected in one move!
 });

 test('should handle gems falling out of bounds', () => {
  // Create a 2x2 board with gem at bottom edge
  const cells = [[Air, Air], [Gem, Air]];
  const target: Position = [0, 0];
  const [board] = createBoard(cells, target);
  const [newBoard, outcome] = applyGravity(Direction.Down, target, board);

  // Gem should stay where it is (can't fall further)
  expect(newBoard.getCell([1, 0])).toBe(Gem);
  expect(outcome).toBe(Outcome.Running);
 });

 test('should detect win when gem reaches target', () => {
  // Create a simple case where gem can reach target in one move
  const cells = [[Gem, Air]];
  const target: Position = [0, 1];
  const [board] = createBoard(cells, target);
  const [newBoard, outcome] = applyGravity(Direction.Right, target, board);

  expect(newBoard.getCell([0, 1])).toBe(Air); // gem disappears when hitting target
  expect(newBoard.getCell([0, 0])).toBe(Air); // gem moved from here
  expect(outcome).toBe(Outcome.Won);
 });

 test('should detect loss when bat reaches target', () => {
  // Create a case where bat reaches target
  const cells = [[Bat, Air]];
  const target: Position = [0, 1];
  const [board] = createBoard(cells, target);
  const [newBoard, outcome] = applyGravity(Direction.Right, target, board);

  expect(newBoard.getCell([0, 1])).toBe(Bat);
  expect(outcome).toBe(Outcome.Lost);
 });

 test('should move gem left with Left gravity', () => {
  // Create a 3x3 board with gem at right
  const cells = [[Air, Air, Gem], [Air, Air, Air], [Air, Air, Air]];
  const target: Position = [2, 2];
  const [board] = createBoard(cells, target);
  const [newBoard, outcome] = applyGravity(Direction.Left, target, board);

  // Gem should move to left column
  expect(newBoard.getCell([0, 0])).toBe(Gem);
  expect(newBoard.getCell([0, 2])).toBe(Air);
  expect(outcome).toBe(Outcome.Running);
 });

 test('should move gem up with Up gravity', () => {
  // Create a 3x3 board with gem at bottom
  const cells = [[Air, Air, Air], [Air, Air, Air], [Gem, Air, Air]];
  const target: Position = [1, 1];
  const [board] = createBoard(cells, target);
  const [newBoard, outcome] = applyGravity(Direction.Up, target, board);

  // Gem should move to top row
  expect(newBoard.getCell([0, 0])).toBe(Gem);
  expect(newBoard.getCell([2, 0])).toBe(Air);
  expect(outcome).toBe(Outcome.Running);
 });

 test('should handle obstacles blocking movement', () => {
  // Create a board with gem blocked by obstacle
  const cells = [[Gem, Obs, Air]];
  const target: Position = [0, 2];
  const [board] = createBoard(cells, target);
  const [newBoard, outcome] = applyGravity(Direction.Right, target, board);

  // Gem should stay in place (blocked by obstacle)
  expect(newBoard.getCell([0, 0])).toBe(Gem);
  expect(newBoard.getCell([0, 1])).toBe(Obs);
  expect(newBoard.getCell([0, 2])).toBe(Air);
  expect(outcome).toBe(Outcome.Running);
 });

 test('should handle multiple movable objects', () => {
  // Create a board with both gem and bat
  const cells = [[Gem, Bat, Air, Air]];
  const target: Position = [0, 3];
  const [board] = createBoard(cells, target);
  const [newBoard, outcome] = applyGravity(Direction.Right, target, board);

  // Both objects should move right, bat reaches target = loss
  expect(newBoard.getCell([0, 0])).toBe(Air);
  expect(newBoard.getCell([0, 1])).toBe(Air);
  expect(newBoard.getCell([0, 2])).toBe(Gem);
  expect(newBoard.getCell([0, 3])).toBe(Bat); // bat at target
  expect(outcome).toBe(Outcome.Lost);
 });

 test('should handle complex chain reactions', () => {
  // Create a board with multiple gems that will chain together
  const cells = [[Gem, Air, Gem, Air]];
  const target: Position = [0, 3];
  const [board] = createBoard(cells, target);
  const [newBoard, outcome] = applyGravity(Direction.Right, target, board);

  // Both gems move right: second gem reaches target first and is collected,
  // then first gem continues all the way to target and is also collected
  expect(newBoard.getCell([0, 0])).toBe(Air);
  expect(newBoard.getCell([0, 1])).toBe(Air);
  expect(newBoard.getCell([0, 2])).toBe(Air);
  expect(newBoard.getCell([0, 3])).toBe(Air); // target remains Air, both gems collected
  expect(outcome).toBe(Outcome.Won); // All gems collected!
 });

 test('should handle gems pushing into obstacles', () => {
  // Test gems that are completely blocked by obstacles
  const cells = [[Gem, Gem, Obs, Air]];
  const target: Position = [0, 3];
  const [board] = createBoard(cells, target);
  const [newBoard, outcome] = applyGravity(Direction.Right, target, board);

  // Both gems are blocked by the obstacle - no movement
  expect(newBoard.getCell([0, 0])).toBe(Gem); // First gem can't move
  expect(newBoard.getCell([0, 1])).toBe(Gem); // Second gem blocked by obstacle
  expect(newBoard.getCell([0, 2])).toBe(Obs); // Obstacle unchanged
  expect(newBoard.getCell([0, 3])).toBe(Air); // Target unchanged
  expect(outcome).toBe(Outcome.Running); // Still have gems left
 });
});
