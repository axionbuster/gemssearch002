import { describe, it, expect } from '@jest/globals';
import { TotM, Gem, Air, applyGravity, Direction, createBoard } from './src/totm';

describe('Debug chain reaction', () => {
  it('should debug what happens with two gems and target', () => {
    // Create a board with two gems in a row
    const cells = [[Gem, Gem, Air]];
    const target = [0, 2] as [number, number];
    const [board] = createBoard(cells, target);
    
    console.log('Initial board:');
    console.log('(0,0):', board.getCell([0, 0]), '(0,1):', board.getCell([0, 1]), '(0,2):', board.getCell([0, 2]));
    
    const [newBoard, outcome] = applyGravity(Direction.Right, target, board);
    
    console.log('After moving Right:');
    console.log('(0,0):', newBoard.getCell([0, 0]), '(0,1):', newBoard.getCell([0, 1]), '(0,2):', newBoard.getCell([0, 2]));
    console.log('Outcome:', outcome);
    
    // Check what actually happened
    expect(newBoard.getCell([0, 0])).toBe(Air);
    console.log('Position (0,1) has:', newBoard.getCell([0, 1]));
    console.log('Position (0,2) has:', newBoard.getCell([0, 2]));
  });
});
