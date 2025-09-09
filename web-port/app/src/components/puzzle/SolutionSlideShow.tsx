import React, { useState, useEffect } from 'react';
import { PuzzleSolution, PuzzleStep } from '@/types/puzzle';
import { PuzzleGridComponent } from './PuzzleGridComponent';
import { Button } from '@/components/ui/button';
import { Separator } from '@/components/ui/separator';
import { Play, Pause, SkipBack, SkipForward, RotateCcw } from 'lucide-react';
import { cn } from '@/lib/utils';

interface SolutionSlideShowProps {
  solution: PuzzleSolution;
  className?: string;
}

export function SolutionSlideShow({ solution, className }: SolutionSlideShowProps) {
  const [currentStep, setCurrentStep] = useState(-1); // -1 for initial state
  const [isPlaying, setIsPlaying] = useState(false);

  const totalSteps = solution.steps.length;
  const currentGrid = currentStep === -1 
    ? solution.initialGrid 
    : solution.steps[currentStep]?.resultingGrid || solution.initialGrid;
  
  const currentStepData = currentStep >= 0 ? solution.steps[currentStep] : null;

  // Remove auto-play functionality - manual navigation only

  const handlePlay = () => {
    if (currentStep >= totalSteps - 1) {
      setCurrentStep(-1); // Reset to beginning
    } else {
      setCurrentStep(prev => prev + 1);
    }
  };

  const handleReset = () => {
    setCurrentStep(-1);
    setIsPlaying(false);
  };

  const handlePrevious = () => {
    setCurrentStep(prev => Math.max(-1, prev - 1));
  };

  const handleNext = () => {
    setCurrentStep(prev => Math.min(totalSteps - 1, prev + 1));
  };

  const getDirectionIcon = (direction: string) => {
    switch (direction.toLowerCase()) {
      case 'up': return '⬆️';
      case 'down': return '⬇️';
      case 'left': return '⬅️';
      case 'right': return '➡️';
      default: return '🔄';
    }
  };

  return (
    <div className={cn('solution-slideshow', className)}>
      {/* Header */}
      <div className="text-center mb-8">
        <h2 className="text-3xl font-bold text-gray-800 mb-2">
          Gem Seeker Solution
        </h2>
        <p className="text-gray-600">
          {solution.solvable ? '✅ Solvable' : '❌ Unsolvable'} • 
          {totalSteps} steps to victory
        </p>
      </div>

      {/* Main Content - Side by Side Layout */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-4 lg:gap-8 items-start">
        
        {/* Left Side - Puzzle Grid */}
        <div className="flex flex-col items-center space-y-2 lg:space-y-4">
          <PuzzleGridComponent
            grid={currentGrid}
            size="lg"
            animated={isPlaying}
          />
          
          {/* Step Indicator */}
          <div className="text-center">
            <div className="text-base lg:text-lg font-semibold text-gray-700 mb-1 lg:mb-2">
              {currentStep === -1 ? "Initial State" : `Step ${currentStep + 1} of ${totalSteps}`}
            </div>
            {currentStepData && (
              <div className="text-xs lg:text-sm text-gray-500 flex items-center justify-center gap-2">
                <span>{getDirectionIcon(currentStepData.direction)}</span>
                <span>{currentStepData.action}</span>
              </div>
            )}
          </div>
        </div>

        {/* Right Side - Steps Overview & Controls */}
        <div className="space-y-3 lg:space-y-6">
          
          {/* Step Navigation */}
          <div className="bg-gray-50 rounded-lg p-3 lg:p-4">
            <h3 className="font-semibold text-gray-800 mb-2 lg:mb-4 text-sm lg:text-base">Solution Steps</h3>
            
            {/* Steps List */}
            <div className="space-y-1 lg:space-y-2 mb-3 lg:mb-4 max-h-32 lg:max-h-48 overflow-y-auto">
              <div 
                className={cn(
                  "flex items-center gap-2 lg:gap-3 p-1.5 lg:p-2 rounded cursor-pointer transition-colors",
                  currentStep === -1 ? "bg-blue-100 border-blue-300" : "hover:bg-gray-100"
                )}
                onClick={() => setCurrentStep(-1)}
              >
                <div className="w-5 h-5 lg:w-6 lg:h-6 rounded-full bg-gray-400 text-white text-xs flex items-center justify-center">
                  0
                </div>
                <span className="text-xs lg:text-sm">Initial puzzle state</span>
              </div>
              
              {solution.steps.map((step, index) => (
                <div
                  key={step.stepNumber}
                  className={cn(
                    "flex items-center gap-2 lg:gap-3 p-1.5 lg:p-2 rounded cursor-pointer transition-colors",
                    currentStep === index ? "bg-blue-100 border-blue-300" : "hover:bg-gray-100"
                  )}
                  onClick={() => setCurrentStep(index)}
                >
                  <div className="w-5 h-5 lg:w-6 lg:h-6 rounded-full bg-blue-500 text-white text-xs flex items-center justify-center">
                    {step.stepNumber}
                  </div>
                  <span className="text-xs lg:text-sm flex items-center gap-1 lg:gap-2">
                    <span>{getDirectionIcon(step.direction)}</span>
                    <span>Apply gravity {step.direction}</span>
                  </span>
                </div>
              ))}
            </div>

            {/* Navigation Controls */}
            <div className="flex items-center justify-between">
              <Button
                variant="outline"
                size="sm"
                onClick={handlePrevious}
                disabled={currentStep <= -1}
              >
                <SkipBack className="h-4 w-4 mr-1" />
                Previous
              </Button>
              
              <Button
                variant="outline"
                size="sm"
                onClick={handleReset}
              >
                <RotateCcw className="h-4 w-4 mr-1" />
                Reset
              </Button>
              
              <Button
                variant="default"
                size="sm"
                onClick={handleNext}
                disabled={currentStep >= totalSteps - 1}
              >
                Next
                <SkipForward className="h-4 w-4 ml-1" />
              </Button>
            </div>
          </div>

          {/* Progress */}
          <div className="space-y-2">
            <div className="flex justify-between text-sm text-gray-500">
              <span>Progress</span>
              <span>{Math.max(0, currentStep + 1)} / {totalSteps + 1}</span>
            </div>
            <div className="w-full bg-gray-200 rounded-full h-2">
              <div
                className="bg-blue-600 h-2 rounded-full transition-all duration-300"
                style={{
                  width: `${((currentStep + 2) / (totalSteps + 1)) * 100}%`
                }}
              />
            </div>
          </div>

          {/* Solution Complete */}
          {currentStep >= totalSteps - 1 && solution.solved && (
            <div className="p-4 bg-green-50 border border-green-200 rounded-lg">
              <div className="text-lg font-bold text-green-800 mb-1">
                🎉 Puzzle Solved!
              </div>
              <div className="text-sm text-green-600">
                All gems collected in {totalSteps} steps!
              </div>
            </div>
          )}
        </div>
      </div>
    </div>
  );
}
