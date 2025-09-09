import React from 'react';
import { PuzzleGrid, CellType } from '@/types/puzzle';
import { PuzzleCell } from './PuzzleCell';
import { cn } from '@/lib/utils';

interface PuzzleGridComponentProps {
  grid: PuzzleGrid;
  size?: 'sm' | 'md' | 'lg';
  animated?: boolean;
  className?: string;
}

export function PuzzleGridComponent({ 
  grid, 
  size = 'md', 
  animated = false, 
  className 
}: PuzzleGridComponentProps) {
  return (
    <div className={cn('puzzle-grid', className)}>
      <div 
        className="border-2 border-gray-300 rounded-lg p-2 bg-white shadow-sm"
        style={{
          display: 'grid',
          gridTemplateColumns: `repeat(${grid.cols}, 1fr)`,
          gridTemplateRows: `repeat(${grid.rows}, 1fr)`,
          gap: '0.25rem'
        }}
      >
        {grid.cells.map((row, rowIndex) =>
          row.split('').map((cell, colIndex) => (
            <PuzzleCell
              key={`${rowIndex}-${colIndex}`}
              cellType={cell as CellType}
              size={size}
              animated={animated}
            />
          ))
        )}
      </div>
      
      {/* Grid info for development */}
      {process.env.NODE_ENV === 'development' && (
        <div className="text-xs text-gray-500 mt-2">
          {grid.rows} × {grid.cols} grid ({grid.cells.reduce((total, row) => total + row.length, 0)} cells)
          <br />
          Expected: {grid.rows * grid.cols} cells
        </div>
      )}
    </div>
  );
}
