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
  const [playbackSpeed, setPlaybackSpeed] = useState(1500); // milliseconds

  const totalSteps = solution.steps.length;
  const currentGrid = currentStep === -1 
    ? solution.initialGrid 
    : solution.steps[currentStep]?.resultingGrid || solution.initialGrid;
  
  const currentStepData = currentStep >= 0 ? solution.steps[currentStep] : null;

  // Auto-play functionality
  useEffect(() => {
    if (!isPlaying) return;

    const interval = setInterval(() => {
      setCurrentStep(prev => {
        if (prev >= totalSteps - 1) {
          setIsPlaying(false);
          return prev;
        }
        return prev + 1;
      });
    }, playbackSpeed);

    return () => clearInterval(interval);
  }, [isPlaying, playbackSpeed, totalSteps]);

  const handlePlay = () => {
    if (currentStep >= totalSteps - 1) {
      setCurrentStep(-1); // Reset to beginning
    }
    setIsPlaying(!isPlaying);
  };

  const handleReset = () => {
    setCurrentStep(-1);
    setIsPlaying(false);
  };

  const handlePrevious = () => {
    setCurrentStep(prev => Math.max(-1, prev - 1));
    setIsPlaying(false);
  };

  const handleNext = () => {
    setCurrentStep(prev => Math.min(totalSteps - 1, prev + 1));
    setIsPlaying(false);
  };

  const getStepDescription = () => {
    if (currentStep === -1) {
      return "Initial puzzle state - Find all the gems! 💎";
    }
    if (currentStepData) {
      return `Step ${currentStepData.stepNumber}: ${currentStepData.action}`;
    }
    return "Unknown step";
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
    <div className={cn('solution-slideshow space-y-6', className)}>
      {/* Header */}
      <div className="text-center space-y-2">
        <h2 className="text-2xl font-bold text-gray-800">
          Gem Seeker Solution
        </h2>
        <p className="text-gray-600">
          {solution.solvable ? '✅ Solvable' : '❌ Unsolvable'} • 
          {totalSteps} steps to victory
        </p>
      </div>

      {/* Main Grid Display */}
      <div className="flex flex-col items-center space-y-4">
        <PuzzleGridComponent
          grid={currentGrid}
          size="lg"
          animated={isPlaying}
          className="transform transition-transform duration-300 hover:scale-105"
        />

        {/* Step Description */}
        <div className="text-center space-y-2">
          <div className="text-lg font-semibold text-gray-700">
            {getStepDescription()}
          </div>
          {currentStepData && (
            <div className="text-sm text-gray-500 flex items-center justify-center gap-2">
              <span>Apply gravity</span>
              <span className="text-xl">
                {getDirectionIcon(currentStepData.direction)}
              </span>
              <span>{currentStepData.direction}</span>
            </div>
          )}
        </div>
      </div>

      <Separator />

      {/* Controls */}
      <div className="flex flex-col space-y-4">
        {/* Playback Controls */}
        <div className="flex items-center justify-center gap-2">
          <Button
            variant="outline"
            size="icon"
            onClick={handleReset}
            disabled={currentStep === -1}
          >
            <RotateCcw className="h-4 w-4" />
          </Button>
          
          <Button
            variant="outline"
            size="icon"
            onClick={handlePrevious}
            disabled={currentStep <= -1}
          >
            <SkipBack className="h-4 w-4" />
          </Button>
          
          <Button
            variant="default"
            onClick={handlePlay}
            className="px-6"
          >
            {isPlaying ? (
              <Pause className="h-4 w-4 mr-2" />
            ) : (
              <Play className="h-4 w-4 mr-2" />
            )}
            {isPlaying ? 'Pause' : currentStep >= totalSteps - 1 ? 'Replay' : 'Play'}
          </Button>
          
          <Button
            variant="outline"
            size="icon"
            onClick={handleNext}
            disabled={currentStep >= totalSteps - 1}
          >
            <SkipForward className="h-4 w-4" />
          </Button>
        </div>

        {/* Progress Bar */}
        <div className="space-y-2">
          <div className="flex justify-between text-sm text-gray-500">
            <span>Step {Math.max(0, currentStep + 1)}</span>
            <span>{totalSteps} total</span>
          </div>
          <div className="w-full bg-gray-200 rounded-full h-2">
            <div
              className="bg-blue-600 h-2 rounded-full transition-all duration-300"
              style={{
                width: `${((currentStep + 1) / totalSteps) * 100}%`
              }}
            />
          </div>
        </div>

        {/* Speed Control */}
        <div className="flex items-center justify-center gap-4 text-sm">
          <span className="text-gray-600">Speed:</span>
          {[
            { label: '0.5x', value: 3000 },
            { label: '1x', value: 1500 },
            { label: '2x', value: 750 }
          ].map(({ label, value }) => (
            <Button
              key={label}
              variant={playbackSpeed === value ? 'default' : 'outline'}
              size="sm"
              onClick={() => setPlaybackSpeed(value)}
            >
              {label}
            </Button>
          ))}
        </div>
      </div>

      {/* Solution Complete */}
      {currentStep >= totalSteps - 1 && solution.solved && (
        <div className="text-center p-4 bg-green-50 border border-green-200 rounded-lg">
          <div className="text-lg font-bold text-green-800">
            🎉 Puzzle Solved! 🎉
          </div>
          <div className="text-sm text-green-600">
            All gems collected in {totalSteps} steps!
          </div>
        </div>
      )}
    </div>
  );
}
