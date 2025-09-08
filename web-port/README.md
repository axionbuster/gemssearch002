# Gem Seeker Web - Pairing Heap Implementation

This folder contains the built web application for the Gem Seeker solver.

## Files

- `index.html` - Interactive demo of the pairing heap
- `index.js` - UMD bundle (works in browsers and Node.js)
- `index.esm.js` - ES module bundle (modern browsers)
- `test.js` - Test suite
- `*.d.ts` - TypeScript declarations

## Usage

Open `index.html` in a web browser or serve this folder with any static file server.

### As a Library

```html
<!-- UMD Bundle -->
<script src="index.js"></script>
<script>
  const heap = new GemSeekerWeb.PairingHeap();
  heap.insert(42);
  console.log(heap.deleteMin()); // 42
</script>
```

```javascript
// ES Module
import { PairingHeap } from './index.esm.js';
const heap = new PairingHeap();
```

## Development

This is a built artifact. Source code is in the parent directory.
