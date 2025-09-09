/**
 * @fileoverview Simple test framework that actually fails when assertions fail
 * @author axionbuster
 * @license BSD-3-Clause
 */

export class TestFailureError extends Error {
 constructor(message: string) {
  super(message);
  this.name = 'TestFailureError';
 }
}

/**
 * Assert that a condition is true, throw error if false
 */
export function assert(condition: boolean, message: string): void {
 if (!condition) {
  throw new TestFailureError(`Assertion failed: ${message}`);
 }
}

/**
 * Assert that two values are equal
 */
export function assertEqual<T>(actual: T, expected: T, message?: string): void {
 const msg = message || `Expected ${expected}, got ${actual}`;
 if (actual !== expected) {
  throw new TestFailureError(`Assertion failed: ${msg}`);
 }
}

/**
 * Assert that two objects are deeply equal (JSON comparison)
 */
export function assertDeepEqual<T>(actual: T, expected: T, message?: string): void {
 const msg = message || `Expected ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`;
 if (JSON.stringify(actual) !== JSON.stringify(expected)) {
  throw new TestFailureError(`Assertion failed: ${msg}`);
 }
}

/**
 * Assert that a value is null
 */
export function assertNull<T>(value: T | null, message?: string): void {
 const msg = message || `Expected null, got ${value}`;
 if (value !== null) {
  throw new TestFailureError(`Assertion failed: ${msg}`);
 }
}

/**
 * Assert that a value is not null
 */
export function assertNotNull<T>(value: T | null, message?: string): void {
 const msg = message || `Expected non-null value, got null`;
 if (value === null) {
  throw new TestFailureError(`Assertion failed: ${msg}`);
 }
}

/**
 * Assert that a function throws an error
 */
export function assertThrows(fn: () => void, message?: string): void {
 const msg = message || 'Expected function to throw an error';
 try {
  fn();
  throw new TestFailureError(`Assertion failed: ${msg}`);
 } catch (error) {
  if (error instanceof TestFailureError) {
   throw error; // Re-throw our own assertion failure
  }
  // Expected behavior - function threw an error
 }
}

/**
 * Test runner that collects and reports results
 */
export class TestRunner {
 private passedTests: string[] = [];
 private failedTests: { name: string; error: Error }[] = [];

 /**
  * Run a single test
  */
 test(name: string, testFn: () => void): void {
  try {
   testFn();
   this.passedTests.push(name);
   console.log(`✓ ${name}`);
  } catch (error) {
   const err = error instanceof Error ? error : new Error(String(error));
   this.failedTests.push({ name, error: err });
   console.error(`✗ ${name}: ${err.message}`);
  }
 }

 /**
  * Run a group of tests with a description
  */
 describe(description: string, testsFn: () => void): void {
  console.log(`\n=== ${description} ===`);
  testsFn();
 }

 /**
  * Get test results summary
  */
 getSummary(): { passed: number; failed: number; total: number; success: boolean } {
  const passed = this.passedTests.length;
  const failed = this.failedTests.length;
  const total = passed + failed;
  return { passed, failed, total, success: failed === 0 };
 }

 /**
  * Print test results summary
  */
 printSummary(): boolean {
  const summary = this.getSummary();
  console.log(`\n=== Test Summary ===`);
  console.log(`Passed: ${summary.passed}`);
  console.log(`Failed: ${summary.failed}`);
  console.log(`Total: ${summary.total}`);

  if (summary.success) {
   console.log('🎉🎉🎉 ALL TESTS PASSED! 🎉🎉🎉');
  } else {
   console.log('❌❌❌ SOME TESTS FAILED ❌❌❌');
   console.log('\nFailures:');
   for (const failure of this.failedTests) {
    console.log(`  ${failure.name}: ${failure.error.message}`);
   }
  }

  return summary.success;
 }

 /**
  * Reset the test runner state
  */
 reset(): void {
  this.passedTests = [];
  this.failedTests = [];
 }
}

// Global test runner instance
export const testRunner = new TestRunner();
