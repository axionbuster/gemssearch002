/**
 * @fileoverview Debug comprehensive tests to understand behavior
 */

import {
  TotM, Air, Bat, Gem, Obs, createBoard, Position, Direction,
  applyGravity, showBoard, Outcome
} from './totm';

describe('Debug comprehensive behavior', () => {
  test('debug vertical movement with obstacles', () => {
    console.log('\n=== DEBUG: Vertical Movement ===');
    const cells = [
      [Air, Obs, Air],
      [Air, Air, Air], 
      [Gem, Obs, Air]
    ];
    const target: Position = [0, 2];
    const [board] = createBoard(cells, target);
    
    console.log('Initial:');
    console.log(showBoard(board, target));
    console.log('Initial cell values:');
    console.log('[0,1]:', board.getCell([0, 1]), '(should be Obs =', Obs, ')');
    console.log('[1,1]:', board.getCell([1, 1]), '(should be Air =', Air, ')');
    console.log('[2,0]:', board.getCell([2, 0]), '(should be Gem =', Gem, ')');
    console.log('[2,1]:', board.getCell([2, 1]), '(should be Obs =', Obs, ')');
    
    const [newBoard, outcome] = applyGravity(Direction.Up, target, board);
    
    console.log('\nAfter Up gravity:');
    console.log(showBoard(newBoard, target));
    console.log('Final cell values:');
    console.log('[0,0]:', newBoard.getCell([0, 0]));
    console.log('[0,1]:', newBoard.getCell([0, 1]));
    console.log('[1,1]:', newBoard.getCell([1, 1]));
    console.log('[2,0]:', newBoard.getCell([2, 0]));
    console.log('[2,1]:', newBoard.getCell([2, 1]));
    console.log('Outcome:', outcome);
    
    expect(true).toBe(true);
  });

  test('debug simultaneous collection', () => {
    console.log('\n=== DEBUG: Simultaneous Collection ===');
    const cells = [
      [Air, Gem, Air],
      [Air, Air, Air],
      [Air, Gem, Air]
    ];
    const target: Position = [1, 1];
    const [board] = createBoard(cells, target);
    
    console.log('Initial:');
    console.log(showBoard(board, target));
    
    const [newBoard, outcome] = applyGravity(Direction.Down, target, board);
    
    console.log('\nAfter Down gravity:');
    console.log(showBoard(newBoard, target));
    console.log('Outcome:', outcome);
    
    expect(true).toBe(true);
  });

  test('debug large board movement', () => {
    console.log('\n=== DEBUG: Large Board ===');
    const cells = [
      [Gem, Air, Obs, Air, Air],
      [Air, Obs, Air, Obs, Air],
      [Air, Air, Air, Air, Gem],
      [Obs, Air, Air, Air, Air],
      [Air, Air, Air, Air, Air]
    ];
    const target: Position = [4, 4];
    const [board] = createBoard(cells, target);
    
    console.log('Initial:');
    console.log(showBoard(board, target));
    
    const [newBoard, outcome] = applyGravity(Direction.Right, target, board);
    
    console.log('\nAfter Right gravity:');
    console.log(showBoard(newBoard, target));
    console.log('Outcome:', outcome);
    
    expect(true).toBe(true);
  });
});
