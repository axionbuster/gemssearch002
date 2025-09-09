import { PuzzleGrid, PuzzleStep, PuzzleSolution, PuzzleCase, GravityDirection } from '@/types/puzzle';

export class PuzzleParser {
 static parseGrid(lines: string[], startLine: number, rows: number): PuzzleGrid {
  const cells: string[] = [];
  let cols = 0;

  for (let i = 0; i < rows; i++) {
   const line = lines[startLine + i] || '';
   cells.push(line);
   // Calculate the maximum column count from actual data
   cols = Math.max(cols, line.length);
  }

  return { rows, cols, cells };
 }

 static parsePuzzleInput(content: string): PuzzleGrid {
  const lines = content.trim().split('\n');
  // const testCases = parseInt(lines[0]);
  const [cols, rows] = lines[1].split(' ').map(Number); // Note: format is cols rows, not rows cols

  return this.parseGrid(lines, 2, rows);
 }

 static parseSolutionOutput(content: string): PuzzleSolution {
  const lines = content.trim().split('\n');
  let lineIndex = 0;

  // Parse test case header
  const testCaseMatch = lines[lineIndex].match(/Test case (\d+):/);
  const testCase = testCaseMatch ? parseInt(testCaseMatch[1]) : 1;
  lineIndex++;

  // Parse solvability
  const solvable = lines[lineIndex].trim() === 'yes';
  lineIndex++;

  // Skip "Initial state:" header
  lineIndex++;

  // Parse initial grid
  const initialGridLines: string[] = [];
  while (lineIndex < lines.length && lines[lineIndex] && !lines[lineIndex].startsWith('Step')) {
   initialGridLines.push(lines[lineIndex]);
   lineIndex++;
  }

  const rows = initialGridLines.length;
  const cols = initialGridLines[0]?.length || 0;
  const initialGrid: PuzzleGrid = {
   rows,
   cols,
   cells: initialGridLines
  };

  // Parse steps
  const steps: PuzzleStep[] = [];

  while (lineIndex < lines.length) {
   const line = lines[lineIndex];

   if (line.startsWith('Step')) {
    // Parse step header like "Step 1: Apply gravity Down"
    const stepMatch = line.match(/Step (\d+): Apply gravity (\w+)/);
    if (stepMatch) {
     const stepNumber = parseInt(stepMatch[1]);
     const direction = stepMatch[2] as GravityDirection;
     lineIndex++;

     // Parse resulting grid
     const stepGridLines: string[] = [];
     while (lineIndex < lines.length && lines[lineIndex] &&
      !lines[lineIndex].startsWith('Step') &&
      !lines[lineIndex].startsWith('Won!')) {
      stepGridLines.push(lines[lineIndex]);
      lineIndex++;
     }

     const resultingGrid: PuzzleGrid = {
      rows: stepGridLines.length,
      cols: stepGridLines[0]?.length || 0,
      cells: stepGridLines
     };

     steps.push({
      stepNumber,
      action: `Apply gravity ${direction}`,
      direction,
      resultingGrid
     });
    }
   } else if (line.startsWith('Won!')) {
    break;
   } else {
    lineIndex++;
   }
  }

  return {
   testCase,
   solvable,
   initialGrid,
   steps,
   solved: lines.some(line => line.trim() === 'Won!')
  };
 }

 static async loadPuzzleCase(caseNumber: number): Promise<PuzzleCase | null> {
  try {
   // In a real implementation, you'd fetch from your server or file system
   // For now, we'll return mock data to demonstrate the structure
   const mockPuzzle: PuzzleGrid = {
    rows: 8,
    cols: 6,
    cells: [
     '@...#@',
     '.#....',
     '......',
     '...#..',
     '#..@.#',
     '......',
     '#*....',
     '..#...'
    ]
   };

   const mockSolution: PuzzleSolution = {
    testCase: 1,
    solvable: true,
    initialGrid: mockPuzzle,
    steps: [
     {
      stepNumber: 1,
      action: 'Apply gravity Down',
      direction: 'Down',
      resultingGrid: {
       rows: 8,
       cols: 6,
       cells: [
        '....#.',
        '.#....',
        '......',
        '@..#.@',
        '#....#',
        '......',
        '#*....',
        '..#@..'
       ]
      }
     },
     {
      stepNumber: 2,
      action: 'Apply gravity Right',
      direction: 'Right',
      resultingGrid: {
       rows: 8,
       cols: 6,
       cells: [
        '....#.',
        '.#....',
        '......',
        '..@#.@',
        '#....#',
        '......',
        '#*....',
        '..#..@'
       ]
      }
     },
     {
      stepNumber: 3,
      action: 'Apply gravity Down',
      direction: 'Down',
      resultingGrid: {
       rows: 8,
       cols: 6,
       cells: [
        '....#.',
        '.#....',
        '......',
        '...#.@',
        '#....#',
        '......',
        '#*@...',
        '..#..@'
       ]
      }
     },
     {
      stepNumber: 4,
      action: 'Apply gravity Left',
      direction: 'Left',
      resultingGrid: {
       rows: 8,
       cols: 6,
       cells: [
        '....#.',
        '.#....',
        '......',
        '...#@.',
        '#....#',
        '......',
        '#*....',
        '..#@..'
       ]
      }
     },
     {
      stepNumber: 5,
      action: 'Apply gravity Up',
      direction: 'Up',
      resultingGrid: {
       rows: 8,
       cols: 6,
       cells: [
        '....#.',
        '.#..@.',
        '......',
        '...#..',
        '#..@.#',
        '......',
        '#*....',
        '..#...'
       ]
      }
     },
     {
      stepNumber: 6,
      action: 'Apply gravity Left',
      direction: 'Left',
      resultingGrid: {
       rows: 8,
       cols: 6,
       cells: [
        '....#.',
        '.#@...',
        '......',
        '...#..',
        '#@...#',
        '......',
        '#*....',
        '..#...'
       ]
      }
     },
     {
      stepNumber: 7,
      action: 'Apply gravity Down',
      direction: 'Down',
      resultingGrid: {
       rows: 8,
       cols: 6,
       cells: [
        '....#.',
        '.#....',
        '......',
        '...#..',
        '#....#',
        '......',
        '#*@...',
        '..#...'
       ]
      }
     },
     {
      stepNumber: 8,
      action: 'Apply gravity Left',
      direction: 'Left',
      resultingGrid: {
       rows: 8,
       cols: 6,
       cells: [
        '....#.',
        '.#....',
        '......',
        '...#..',
        '#....#',
        '......',
        '#*....',
        '..#...'
       ]
      }
     }
    ],
    solved: true
   };

   return {
    caseNumber,
    puzzle: mockPuzzle,
    solutions: [mockSolution]
   };
  } catch (error) {
   console.error(`Failed to load puzzle case ${caseNumber}:`, error);
   return null;
  }
 }
}
