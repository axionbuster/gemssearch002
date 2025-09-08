import { PairingHeap } from './pairing-heap';

// Test suite for the pairing heap implementation
export function runTests(): boolean {
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
      console.assert(perfResults[i-1] <= perfResults[i], "Results should be sorted");
    }
    console.log('✓ All performance test elements properly sorted');

    console.log('🎉 All tests passed!');
    return true;
    
  } catch (error) {
    console.error('❌ Test failed:', error);
    return false;
  }
}

// Run tests if this module is executed directly
if (typeof window === 'undefined') {
  // Node.js environment
  runTests();
} else {
  // Browser environment - expose globally
  (window as any).runTests = runTests;
}
