'use client';

import React, { useState, useEffect } from 'react';
import { PuzzleCase } from '@/types/puzzle';
import { PuzzleParser } from '@/lib/puzzle-parser';
import { SolutionSlideShow } from '@/components/puzzle/SolutionSlideShow';
import { PuzzleNavigation } from '@/components/puzzle/PuzzleNavigation';

export default function Home() {
  const [currentCase, setCurrentCase] = useState<PuzzleCase | null>(null);
  const [totalCases, setTotalCases] = useState(1);
  const [loading, setLoading] = useState(true);

  // Load initial case
  useEffect(() => {
    loadCase(0);
  }, []);

  const loadCase = async (caseNumber: number) => {
    setLoading(true);
    try {
      const puzzleCase = await PuzzleParser.loadPuzzleCase(caseNumber);
      setCurrentCase(puzzleCase);
      setTotalCases(1); // For now, we only have mock data
    } catch (error) {
      console.error('Failed to load case:', error);
    } finally {
      setLoading(false);
    }
  };

  const handleNavigate = (caseNumber: number) => {
    loadCase(caseNumber);
  };

  const handleHome = () => {
    loadCase(0); // Load latest case (for now, case 0)
  };

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center space-y-4">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto"></div>
          <div className="text-gray-600">Loading Gem Seeker puzzle...</div>
        </div>
      </div>
    );
  }

  if (!currentCase) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center space-y-4">
          <div className="text-xl text-gray-600">No puzzle found</div>
          <div className="text-sm text-gray-500">
            Please check your puzzle data files.
          </div>
        </div>
      </div>
    );
  }

  const currentSolution = currentCase.solutions[0]; // For now, show first solution

  return (
    <div className="min-h-screen bg-gray-50">
      {/* Navigation */}
      <PuzzleNavigation
        currentCase={currentCase}
        totalCases={totalCases}
        onNavigate={handleNavigate}
        onHome={handleHome}
      />

      {/* Main Content */}
      <main className="container mx-auto px-4 py-8 max-w-4xl">
        {currentSolution ? (
          <SolutionSlideShow solution={currentSolution} />
        ) : (
          <div className="text-center space-y-4">
            <div className="text-xl text-gray-600">No solution available</div>
            <div className="text-sm text-gray-500">
              This puzzle hasn't been solved yet.
            </div>
          </div>
        )}
      </main>

      {/* Footer */}
      <footer className="border-t border-gray-200 bg-white py-8 mt-16">
        <div className="container mx-auto px-4 text-center space-y-4">
          <div className="text-lg font-semibold text-gray-800">
            Gem Seeker Solver
          </div>
          <div className="text-sm text-gray-600">
            Interactive puzzle solutions and visualization
          </div>
          <div className="text-xs text-gray-500">
            Built with Next.js, React, and Radix UI
          </div>
          {/* Placeholder for future attribution/contact info */}
          <div className="text-xs text-gray-400">
            Space reserved for attribution and contacts
          </div>
        </div>
      </footer>
    </div>
  );
}
