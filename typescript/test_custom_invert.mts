#!/usr/bin/env node
/*********************************************************************
 *  test_custom_invert.mts: test a custom TypeScript function
 *********************************************************************/

import fc     from 'fast-check';
import fs     from 'node:fs/promises';
import os     from 'node:os';
import path   from 'node:path';
import url    from 'node:url';
import assert from 'node:assert';

export type Nat  = number;
export type Bit  = false | true;
export type Tree<A = Nat> = [Tree<A>, Tree<A>] | A;


function buildPerfectTree(depth: number, start = 0): [Tree<number>, number] {
  if (depth === 0) return [start, start + 1];
  const [l, n1] = buildPerfectTree(depth - 1, start);
  const [r, n2] = buildPerfectTree(depth - 1, n1);
  return [[l, r], n2];
}

function treeFromLeaves<T>(depth: number, leaves: T[], i = 0): [Tree<T>, number] {
  if (depth === 0) return [leaves[i], i + 1];
  const [l, n1] = treeFromLeaves(depth - 1, leaves, i);
  const [r, n2] = treeFromLeaves(depth - 1, leaves, n1);
  return [[l, r], n2];
}

const flatten = <T,>(t: Tree<T>): T[] =>
  Array.isArray(t) ? [...flatten(t[0]), ...flatten(t[1])] : [t];

const reverseBits = (n: number, bits: number) => {
  let r = 0;
  for (let i = 0; i < bits; ++i) r = (r << 1) | ((n >> i) & 1);
  return r;
};

const bitReverse = <T,>(xs: T[]) => {
  const n = xs.length, bits = Math.log2(n), out = new Array<T>(n);
  for (let i = 0; i < n; ++i) out[reverseBits(i, bits)] = xs[i];
  return out;
};

/* ASCII‑art tree ─ style: Node, +--, \--, |                         */
function asciiTree<T>(t: Tree<T>): string {
  const walk = (node: Tree<T>, prefix: string, isLeft: boolean, isRoot = false): string[] => {
    const label = Array.isArray(node) ? 'Node' : `Leaf(${node})`;
    const thisLine = isRoot ? label : prefix + (isLeft ? '+-- ' : '\\-- ') + label;
    if (!Array.isArray(node)) return [thisLine];

    const childPrefix = isRoot ? '' : prefix + (isLeft ? '|   ' : '    ');
    const [left, right] = node;
    return [
      thisLine,
      ...walk(left,  childPrefix, true),
      ...walk(right, childPrefix, false)
    ];
  };
  return walk(t, '', true, true).join('\n');
}

/* ─── load candidate implementation (file or paste) ─────────────── */
async function loadModule(): Promise<any> {
  if (process.argv[2]) {
    const file = path.resolve(process.argv[2]);
    return import(url.pathToFileURL(file).href);
  }

  process.stderr.write('Paste your TypeScript implementation (Ctrl‑D to end)\n');
  const code: string = await new Promise((res, rej) => {
    let s = ''; process.stdin.setEncoding('utf8');
    process.stdin.on('data',  c => (s += c));
    process.stdin.on('end',   () => res(s));
    process.stdin.on('error', rej);
  });

  let prefix = '';
  if (!/\btype\s+Nat\b/.test(code))  prefix += 'type Nat = number;\n';
  if (!/\btype\s+Bit\b/.test(code))  prefix += 'type Bit = false | true;\n';
  if (!/\btype\s+Tree\b/.test(code))
    prefix += 'type Tree<A = Nat> = [Tree<A>, Tree<A>] | A;\n';

  const dir  = await fs.mkdtemp(path.join(os.tmpdir(), 'invert_'));
  const file = path.join(dir, 'invert.ts');
  await fs.writeFile(file, prefix + code, 'utf8');
  const mod  = await import(url.pathToFileURL(file).href);
  await fs.rm(dir, { recursive: true, force: true });
  return mod;
}

const MAX_DEPTH = 4;

(async () => {
  const { invert } = await loadModule();
  if (typeof invert !== 'function')
    throw new Error("No function named 'invert' exported.");

  const depthArb = fc.integer({ min: 0, max: MAX_DEPTH });

  fc.assert(
    fc.property(depthArb, (d) => {
      const [tree]       = buildPerfectTree(d);
      const originalFlat = flatten(tree);

      const invertedTree =
        invert.length === 1 ? invert(tree) : invert(true, tree);
      const invertedFlat = flatten(invertedTree);
      const expectedFlat = bitReverse(originalFlat);
      const [expectedTree] = treeFromLeaves(d, expectedFlat);

      /* first differing position */
      let diff = -1;
      for (let i = 0; i < expectedFlat.length; ++i)
        if (expectedFlat[i] !== invertedFlat[i]) { diff = i; break; }

      assert.deepStrictEqual(
        invertedFlat,
        expectedFlat,
        [
          `Failed at depth = ${d}`,
          diff >= 0
            ? `First differing index ${diff}: expected ${expectedFlat[diff]}, got ${invertedFlat[diff]}`
            : '(difference location not found)',
          '\nOriginal tree:\n' + asciiTree(tree),
          '\nExpected bit‑reversed tree:\n' + asciiTree(expectedTree),
          '\nYour inverted tree:\n' + asciiTree(invertedTree),
          '\nFlattened arrays:',
          `  Original : ${JSON.stringify(originalFlat)}`,
          `  Expected : ${JSON.stringify(expectedFlat)}`,
          `  Got      : ${JSON.stringify(invertedFlat)}`
        ].join('\n')
      );
    }),
    { numRuns: 100, verbose: true, endOnFailure: true }
  );

  console.log('\n✅  All tests passed! 🎉');
})().catch(e => { console.error('\n❌  ', e); process.exitCode = 1; });
