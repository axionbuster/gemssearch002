export type CellType = '@' | '#' | '*' | '%' | '.';

export type GravityDirection = 'Up' | 'Down' | 'Left' | 'Right';

export interface PuzzleGrid {
 rows: number;
 cols: number;
 cells: string[];
}

export interface PuzzleStep {
 stepNumber: number;
 action: string;
 direction: GravityDirection;
 resultingGrid: PuzzleGrid;
}

export interface PuzzleSolution {
 testCase: number;
 solvable: boolean;
 initialGrid: PuzzleGrid;
 steps: PuzzleStep[];
 solved: boolean;
}

export interface PuzzleCase {
 caseNumber: number;
 puzzle: PuzzleGrid;
 solutions: PuzzleSolution[];
}

export interface PuzzleDatabase {
 cases: PuzzleCase[];
 latestCase?: PuzzleCase;
}
