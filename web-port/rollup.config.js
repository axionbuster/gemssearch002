import typescript from '@rollup/plugin-typescript';
import { nodeResolve } from '@rollup/plugin-node-resolve';
import copy from 'rollup-plugin-copy';

export default [
  // Main library bundle
  {
    input: 'src/index.ts',
    output: [
      {
        file: 'dist/index.js',
        format: 'umd',
        name: 'GemSeekerWeb',
        sourcemap: true
      },
      {
        file: 'dist/index.esm.js', 
        format: 'esm',
        sourcemap: true
      }
    ],
    plugins: [
      nodeResolve(),
      typescript({
        tsconfig: './tsconfig.json',
        declaration: true,
        declarationDir: 'dist'
      }),
      copy({
        targets: [
          { src: 'README.md', dest: 'dist' }
        ]
      })
    ]
  },
  
  // Test bundle (for running tests in browser/node)
  {
    input: 'src/test.ts',
    output: {
      file: 'dist/test.js',
      format: 'iife',
      name: 'Tests',
      sourcemap: true
    },
    plugins: [
      nodeResolve(),
      typescript({
        tsconfig: './tsconfig.json'
      })
    ]
  },
  
  // TotM optimization test bundle
  {
    input: 'src/totm-test.ts',
    output: {
      file: 'dist/totm-test.js',
      format: 'iife',
      name: 'TotMTest',
      sourcemap: true
    },
    plugins: [
      nodeResolve(),
      typescript({
        tsconfig: './tsconfig.json'
      })
    ]
  }
];
