import { PairingHeap } from './pairing-heap';
import { dijk, recon } from './dijk';

// Test suite for the pairing heap implementation
export function runPairingHeapTests(): boolean {
 console.log('Starting Pairing Heap Tests...');

 try {
  // Test 1: Empty heap
  console.log('Test 1: Empty heap operations');
  const emptyHeap = new PairingHeap<number>();
  console.assert(emptyHeap.isEmpty() === true, "Empty heap should be empty");
  console.assert(emptyHeap.peek() === null, "Empty heap peek should return null");
  console.assert(emptyHeap.deleteMin() === null, "Empty heap deleteMin should return null");
  console.log('✓ Empty heap tests passed');

  // Test 2: Single element
  console.log('Test 2: Single element heap');
  const singleHeap = new PairingHeap<number>(42);
  console.assert(singleHeap.peek() === 42, "Single element heap peek");
  console.assert(singleHeap.deleteMin() === 42, "Single element heap deleteMin");
  console.assert(singleHeap.isEmpty() === true, "Single element heap should be empty after deleteMin");
  console.log('✓ Single element tests passed');

  // Test 3: Multiple elements - priority queue behavior
  console.log('Test 3: Priority queue behavior');
  const heap = new PairingHeap<number>();
  const values = [5, 3, 8, 1, 6, 2, 7, 4];

  // Insert all values
  values.forEach(v => heap.insert(v));
  console.assert(heap.peek() === 1, "Min should be 1");

  // Extract all elements - should come out sorted
  const results: number[] = [];
  while (!heap.isEmpty()) {
   const min = heap.deleteMin();
   if (min !== null) results.push(min);
  }

  const expected = [1, 2, 3, 4, 5, 6, 7, 8];
  console.assert(JSON.stringify(results) === JSON.stringify(expected),
   `Expected ${expected}, got ${results}`);
  console.log('✓ Priority queue behavior tests passed');

  // Test 4: Large dataset performance test
  console.log('Test 4: Performance test with 1000 elements');
  const perfHeap = new PairingHeap<number>();
  const perfStart = performance.now();

  // Insert 1000 random elements
  for (let i = 0; i < 1000; i++) {
   perfHeap.insert(Math.floor(Math.random() * 10000));
  }

  // Extract all elements
  const perfResults: number[] = [];
  while (!perfHeap.isEmpty()) {
   const min = perfHeap.deleteMin();
   if (min !== null) perfResults.push(min);
  }

  const perfEnd = performance.now();
  console.log(`✓ Performance test completed in ${perfEnd - perfStart}ms`);

  // Verify sorted order
  for (let i = 1; i < perfResults.length; i++) {
   console.assert(perfResults[i - 1] <= perfResults[i], "Results should be sorted");
  }
  console.log('✓ All performance test elements properly sorted');

  console.log('🎉 All pairing heap tests passed!');
  return true;

 } catch (error) {
  console.error('❌ Pairing heap test failed:', error);
  return false;
 }
}

// Test suite for Dijkstra's algorithm
export function runDijkstraTests(): boolean {
 console.log('Starting Dijkstra Tests...');

 try {
  // Simple grid world test
  console.log('Test 1: Simple grid pathfinding');

  // State: just a position [x, y]
  type Position = [number, number];
  enum Direction { Up, Down, Left, Right }

  // Define a simple 3x3 grid: 
  // [0,0] [1,0] [2,0]
  // [0,1] [1,1] [2,1]  
  // [0,2] [1,2] [2,2]

  const weight = (from: Position, to: Position): number => 1; // uniform cost

  function* neighbors(pos: Position): Iterable<[Direction, Position]> {
   const [x, y] = pos;
   const moves: [Direction, Position][] = [
    [Direction.Up, [x, y - 1]],
    [Direction.Down, [x, y + 1]],
    [Direction.Left, [x - 1, y]],
    [Direction.Right, [x + 1, y]]
   ];

   for (const [dir, [newX, newY]] of moves) {
    // Simple bounds check for 3x3 grid
    if (newX >= 0 && newX < 3 && newY >= 0 && newY < 3) {
     yield [dir, [newX, newY]];
    }
   }
  }

  const isGoal = (pos: Position): boolean => pos[0] === 2 && pos[1] === 2;
  const start: Position = [0, 0];

  const result = dijk(weight, neighbors, isGoal, start);

  console.assert(result.target !== null, "Should find target");
  console.assert(result.target![0] === 2 && result.target![1] === 2, "Target should be [2,2]");

  if (result.target) {
   const [path, moves, cost] = recon(result, result.target);
   console.assert(cost === 4, `Expected cost 4, got ${cost}`); // Manhattan distance
   console.assert(moves.length === 4, `Expected 4 moves, got ${moves.length}`);
   console.log(`✓ Found path with cost ${cost} and ${moves.length} moves`);
  }

  // Test 2: No path scenario
  console.log('Test 2: No path scenario');

  function* noNeighbors(pos: Position): Iterable<[Direction, Position]> {
   // No neighbors - isolated start state
   if (false) yield [Direction.Up, [0, 0]]; // Never reached, just for TypeScript
  }

  const noPathResult = dijk(weight, noNeighbors, isGoal, start);
  console.assert(noPathResult.target === null, "Should not find target when no path exists");
  console.log('✓ Correctly handled no-path scenario');

  // Test 3: Start state is goal
  console.log('Test 3: Start state is goal');

  const alwaysTrue = (pos: Position): boolean => true;
  const immediateResult = dijk(weight, neighbors, alwaysTrue, start);

  console.assert(immediateResult.target !== null, "Should find target immediately");
  console.assert(
   immediateResult.target![0] === start[0] && immediateResult.target![1] === start[1],
   "Target should be start state"
  );

  if (immediateResult.target) {
   const [path, moves, cost] = recon(immediateResult, immediateResult.target);
   console.assert(cost === 0, `Expected cost 0, got ${cost}`);
   console.assert(moves.length === 0, `Expected 0 moves, got ${moves.length}`);
   console.log('✓ Correctly handled start-is-goal scenario');
  }

  console.log('🎉 All Dijkstra tests passed!');
  return true;

 } catch (error) {
  console.error('❌ Dijkstra test failed:', error);
  return false;
 }
}

// Combined test runner
export function runAllTests(): boolean {
 console.log('=== Running All Tests ===');
 const heapSuccess = runPairingHeapTests();
 const dijkstraSuccess = runDijkstraTests();

 if (heapSuccess && dijkstraSuccess) {
  console.log('🎉🎉🎉 ALL TESTS PASSED! 🎉🎉🎉');
  return true;
 } else {
  console.log('❌❌❌ SOME TESTS FAILED ❌❌❌');
  return false;
 }
}

// Run tests if this module is executed directly
if (typeof window === 'undefined') {
 // Node.js environment
 runAllTests();
} else {
 // Browser environment - expose globally
 (window as any).runAllTests = runAllTests;
 (window as any).runPairingHeapTests = runPairingHeapTests;
 (window as any).runDijkstraTests = runDijkstraTests;
}
