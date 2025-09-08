type N<A> = H<A> | null
class H<A> {
 a: A
 p: N<A>
 l: N<A>
 r: N<A>
 constructor(a: A, p: N<A>, l: N<A>, r: N<A>) {
  this.a = a
  this.p = p
  this.l = l
  this.r = r
 }
}
// Helper type for pairing operation  
type Pair<A> = { kind: 'two', x: H<A>, y: H<A> } | { kind: 'one', x: H<A> }

class PairingHeap<A> {
 root: N<A>

 constructor(data?: A) {
  this.root = data !== undefined ? heap1(data) : null
 }

 static fromNode<A>(node: N<A>): PairingHeap<A> {
  const heap = new PairingHeap<A>()
  heap.root = node
  return heap
 }

 isEmpty(): boolean {
  return this.root === null
 }

 peek(): A | null {
  return this.root ? this.root.a : null
 }

 insert(value: A): void {
  const newNode = heap1(value)
  this.root = this.root ? meld(newNode, this.root) : newNode
 }

 deleteMin(): A | null {
  if (!this.root) return null

  const minValue = this.root.a

  // Collect children into a list
  const children: H<A>[] = []
  if (this.root.l) children.push(this.root.l)
  if (this.root.r) children.push(this.root.r)

  // Pair up children and merge
  this.root = mergePairs(children)

  return minValue
 }
}
function heap0<A>(): N<A> { return null }
function heap1<A>(a: A): H<A> { return new H(a, null, null, null) }

// Meld two heaps - the core operation of pairing heaps
function meld<A>(h1: N<A>, h2: N<A>): N<A> {
 if (!h1) return h2
 if (!h2) return h1

 // Ensure h1 has the smaller root
 if (h1.a > h2.a) {
  const temp = h1
  h1 = h2
  h2 = temp
 }

 // Make h2 the leftmost child of h1
 h2.p = h1
 h2.r = h1.l
 if (h1.l) h1.l.p = h2
 h1.l = h2

 return h1
}

// Merge a list of heaps using the two-pass algorithm
function mergePairs<A>(heaps: H<A>[]): N<A> {
 if (heaps.length === 0) return null
 if (heaps.length === 1) return heaps[0]

 // First pass: pair up adjacent heaps left to right
 const paired: H<A>[] = []
 for (let i = 0; i < heaps.length; i += 2) {
  if (i + 1 < heaps.length) {
   // Pair exists
   paired.push(meld(heaps[i], heaps[i + 1])!)
  } else {
   // Odd heap out
   paired.push(heaps[i])
  }
 }

 // Second pass: merge right to left
 let result = paired[paired.length - 1]
 for (let i = paired.length - 2; i >= 0; i--) {
  result = meld(paired[i], result)!
 }

 return result
}

function link<A>(h: H<A>, g: H<A>) {
 h.a < g.a ? g.p = h : h.p = g
}

// Additional utility functions for complete pairing heap implementation
function insert<A>(value: A, heap: N<A>): H<A> {
 const newNode = heap1(value)
 return meld(newNode, heap)!
}

// For decrease-key operation, you'd typically need to:
// 1. Find the node (requires additional bookkeeping)
// 2. Decrease its value
// 3. Cut it from its parent and meld with root
function decreaseKey<A>(node: H<A>, newValue: A, heap: PairingHeap<A>): void {
 node.a = newValue

 // If it's not the root, cut it and meld with root
 if (node.p) {
  // Remove from parent's children
  if (node.p.l === node) {
   node.p.l = node.r
  } else {
   // Find in sibling chain and remove
   let prev = node.p.l
   while (prev && prev.r !== node) {
    prev = prev.r
   }
   if (prev) prev.r = node.r
  }

  if (node.r) node.r.p = node.p

  // Clear parent reference and meld with root
  node.p = null
  node.r = null
  heap.root = meld(node, heap.root)
 }
}

// Example usage and testing
function testPairingHeap() {
 const heap = new PairingHeap<number>()

 // Insert some values
 heap.insert(5)
 heap.insert(3)
 heap.insert(8)
 heap.insert(1)
 heap.insert(6)

 console.log("Min:", heap.peek()) // Should be 1

 // Extract all elements (should come out in sorted order)
 const results: number[] = []
 while (!heap.isEmpty()) {
  const min = heap.deleteMin()
  if (min !== null) results.push(min)
 }

 console.log("Sorted:", results) // Should be [1, 3, 5, 6, 8]
}