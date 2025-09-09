# Gem Seeker Web Solver

A modern, responsive web application for visualizing and interacting with Gem Seeker puzzle solutions. Built with Next.js, React, TypeScript, and Radix UI.

## Features

- 🎮 **Interactive Solution Playback**: Step-by-step animated visualization of puzzle solutions
- 📱 **Mobile Responsive**: Optimized for both desktop and mobile viewing
- 🧩 **Puzzle Grid Rendering**: Visual representation of gems, walls, targets, and empty spaces
- ⏯️ **Playback Controls**: Play, pause, step forward/backward, and adjust playback speed
- 🗂️ **Historical Navigation**: Browse through different puzzle cases
- 🎨 **Modern UI**: Clean, accessible design using Radix UI components

## Project Structure

```
src/
├── app/                    # Next.js app router pages
├── components/
│   ├── ui/                # Reusable UI components (Button, Separator)
│   └── puzzle/            # Puzzle-specific components
│       ├── PuzzleCell.tsx         # Individual puzzle cell rendering
│       ├── PuzzleGridComponent.tsx # Complete puzzle grid
│       ├── SolutionSlideShow.tsx  # Interactive solution viewer
│       └── PuzzleNavigation.tsx   # Case navigation
├── lib/
│   ├── puzzle-parser.ts   # Puzzle data parsing utilities
│   └── utils.ts          # General utility functions
└── types/
    └── puzzle.ts         # TypeScript type definitions
```

## Puzzle Data Format

### Input Files (`caseN.txt`)
```
1               # Number of test cases
6 8             # Grid dimensions (rows cols)
@...#@          # Grid data with symbols:
.#....          #   @ = gems
......          #   # = walls  
...#..          #   * = target
#..@.#          #   % = special gems
......          #   . = empty space
```

### Solution Files (`caseN_outM.txt`)
```
Test case 1:
yes             # Solvable (yes/no)
Initial state:
[grid]          # Starting puzzle state
Step 1: Apply gravity Down
[grid]          # Grid after step 1
...             # More steps
Won!            # Success indicator
```

## Development

### Prerequisites
- Node.js 18+ 
- npm or yarn

### Getting Started

1. Install dependencies:
```bash
npm install
```

2. Start development server:
```bash
npm run dev
```

3. Open [http://localhost:3000](http://localhost:3000) in your browser

### Available Scripts

- `npm run dev` - Start development server with Turbopack
- `npm run build` - Build for production
- `npm run start` - Start production server
- `npm run lint` - Run ESLint

## Technology Stack

- **Frontend Framework**: Next.js 15 with App Router
- **Language**: TypeScript
- **UI Library**: Radix UI
- **Styling**: Tailwind CSS  
- **Icons**: Lucide React
- **Build Tool**: Turbopack

## Future Development Plans

### Phase 1 (Current)
- ✅ Passive puzzle renderer and solution viewer
- ✅ Mobile-responsive interface
- ✅ Interactive solution playback

### Phase 2 (Planned)
- 🔄 Desktop puzzle designer
- 🔄 Drag & drop puzzle creation
- 🔄 Real-time solver integration
- 🔄 Puzzle validation and export

## Contributing

This project is currently in active development. The codebase is designed to be:
- Modular and component-based
- Type-safe with TypeScript
- Accessible and responsive
- Easily extensible for new features

## Data Integration

Currently using mock data for demonstration. To integrate with actual Haskell solver output:

1. Implement file reading in `PuzzleParser.loadPuzzleCase()`
2. Add API endpoints for puzzle data if needed
3. Update data types as needed for additional puzzle formats

## License

BSD-3-Clause License

Copyright (c) 2025, axionbuster
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
