import React from 'react';
import { PuzzleCase } from '@/types/puzzle';
import { Button } from '@/components/ui/button';
import { Separator } from '@/components/ui/separator';
import { ChevronLeft, ChevronRight, Home, Calendar } from 'lucide-react';
import { cn } from '@/lib/utils';

interface PuzzleNavigationProps {
  currentCase: PuzzleCase | null;
  totalCases: number;
  onNavigate: (caseNumber: number) => void;
  onHome: () => void;
  className?: string;
}

export function PuzzleNavigation({
  currentCase,
  totalCases,
  onNavigate,
  onHome,
  className
}: PuzzleNavigationProps) {
  const currentCaseNumber = currentCase?.caseNumber || 0;
  const canGoPrevious = currentCaseNumber > 0;
  const canGoNext = currentCaseNumber < totalCases - 1;

  const handlePrevious = () => {
    if (canGoPrevious) {
      onNavigate(currentCaseNumber - 1);
    }
  };

  const handleNext = () => {
    if (canGoNext) {
      onNavigate(currentCaseNumber + 1);
    }
  };

  return (
    <div className={cn('puzzle-navigation', className)}>
      <div className="flex items-center justify-between p-4 bg-white border-b border-gray-200">
        {/* Home Button */}
        <Button
          variant="outline"
          onClick={onHome}
          className="flex items-center gap-2"
        >
          <Home className="h-4 w-4" />
          <span className="hidden sm:inline">Latest</span>
        </Button>

        {/* Case Info */}
        <div className="flex items-center gap-4">
          <div className="text-center">
            <div className="text-lg font-bold text-gray-800">
              Case #{currentCaseNumber}
            </div>
            <div className="text-xs text-gray-500 flex items-center gap-1">
              <Calendar className="h-3 w-3" />
              {currentCase?.solutions?.length || 0} solution(s)
            </div>
          </div>
        </div>

        {/* Navigation Controls */}
        <div className="flex items-center gap-2">
          <Button
            variant="outline"
            size="icon"
            onClick={handlePrevious}
            disabled={!canGoPrevious}
          >
            <ChevronLeft className="h-4 w-4" />
          </Button>
          
          <div className="text-sm text-gray-600 min-w-[60px] text-center">
            {currentCaseNumber + 1} / {totalCases}
          </div>
          
          <Button
            variant="outline"
            size="icon"
            onClick={handleNext}
            disabled={!canGoNext}
          >
            <ChevronRight className="h-4 w-4" />
          </Button>
        </div>
      </div>

      {/* Progress Bar */}
      <div className="h-1 bg-gray-100">
        <div
          className="h-full bg-blue-500 transition-all duration-300"
          style={{
            width: totalCases > 0 ? `${((currentCaseNumber + 1) / totalCases) * 100}%` : '0%'
          }}
        />
      </div>
    </div>
  );
}
