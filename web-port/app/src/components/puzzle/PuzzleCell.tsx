import React from 'react';
import { CellType } from '@/types/puzzle';
import { cn } from '@/lib/utils';

interface PuzzleCellProps {
  cellType: CellType;
  size?: 'sm' | 'md' | 'lg';
  animated?: boolean;
}

const getCellContent = (cellType: CellType): { emoji: string; bgColor: string; textColor: string } => {
  switch (cellType) {
    case '@':
      return { emoji: '💎', bgColor: 'bg-blue-100', textColor: 'text-blue-600' };
    case '#':
      return { emoji: '🧱', bgColor: 'bg-stone-700', textColor: 'text-stone-100' };
    case '*':
      return { emoji: '🎯', bgColor: 'bg-red-100', textColor: 'text-red-600' };
    case '%':
      return { emoji: '🦇', bgColor: 'bg-purple-200', textColor: 'text-purple-700' };
    case '.':
    default:
      return { emoji: '', bgColor: 'bg-gray-50', textColor: 'text-gray-400' };
  }
};

const getSizeClasses = (size: 'sm' | 'md' | 'lg') => {
  switch (size) {
    case 'sm':
      return 'w-6 h-6 text-xs';
    case 'lg':
      return 'w-12 h-12 text-lg';
    case 'md':
    default:
      return 'w-8 h-8 text-sm';
  }
};

export function PuzzleCell({ cellType, size = 'md', animated = false }: PuzzleCellProps) {
  const { emoji, bgColor, textColor } = getCellContent(cellType);
  const sizeClasses = getSizeClasses(size);

  return (
    <div
      className={cn(
        'flex items-center justify-center border border-gray-200 rounded-sm font-mono transition-all duration-300',
        bgColor,
        textColor,
        sizeClasses,
        animated && 'animate-pulse'
      )}
    >
      {emoji || cellType}
    </div>
  );
}
