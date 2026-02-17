#!/usr/bin/env node

// Generates llms-full.txt by combining llms.txt with the complete API reference from api.json.
// Usage: node scripts/generate-llms-full.js

import { readFileSync, writeFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const siteRoot = join(__dirname, '..');

const llmsTxt = readFileSync(join(siteRoot, '..', 'llms.txt'), 'utf-8');
const apiData = JSON.parse(readFileSync(join(siteRoot, 'src', 'lib', 'data', 'api.json'), 'utf-8'));

// Category display names and ordering
const CATEGORIES = [
  { key: 'language', heading: 'Language' },
  { key: 'web', heading: 'Web Framework' },
  { key: 'database', heading: 'Database' },
];

function formatSignature(mod, fn) {
  const name = fn.name;
  // Parse "fn funcname(args) -> Return" into just "(args) -> Return"
  const sig = fn.signature;
  const parenIdx = sig.indexOf('(');
  if (parenIdx === -1) {
    return `${mod.name}.${name}`;
  }
  const rest = sig.slice(parenIdx); // "(args) -> Return"
  return `${mod.name}.${name}${rest}`;
}

function formatExample(example) {
  if (!example) return '';
  // Collapse multi-line examples to single line for compact display, or keep short ones
  const lines = example.split('\n');
  if (lines.length === 1) {
    return `\`${example}\``;
  }
  return '```\n' + example + '\n```';
}

function buildApiReference(modules) {
  const lines = [];
  lines.push('## Standard Library Reference');
  lines.push('');
  lines.push('Complete API for every module. Functions marked with `!` are effectful.');
  lines.push('');

  for (const cat of CATEGORIES) {
    const catModules = modules
      .filter(m => m.category === cat.key)
      .sort((a, b) => a.name.localeCompare(b.name));

    if (catModules.length === 0) continue;

    lines.push(`### ${cat.heading}`);
    lines.push('');

    for (const mod of catModules) {
      lines.push(`#### ${mod.name}`);
      if (mod.description) {
        lines.push(mod.description);
      }
      lines.push('');

      for (const fn of mod.functions) {
        const sig = formatSignature(mod, fn);
        let entry = `- \`${sig}\` — ${fn.description}`;
        if (fn.example) {
          const exLines = fn.example.split('\n');
          if (exLines.length === 1) {
            entry += `  \n  Example: \`${fn.example}\``;
          } else {
            entry += '\n  Example:\n  ```\n  ' + exLines.join('\n  ') + '\n  ```';
          }
        }
        lines.push(entry);
      }
      lines.push('');
    }
  }

  return lines.join('\n');
}

// Split llms.txt: insert API reference between "## Standard Library" section and "## Common Patterns"
const insertBefore = '## Common Patterns';
const splitIdx = llmsTxt.indexOf(insertBefore);

if (splitIdx === -1) {
  console.error('ERROR: Could not find "## Common Patterns" section in llms.txt');
  process.exit(1);
}

const before = llmsTxt.slice(0, splitIdx).trimEnd();
const after = llmsTxt.slice(splitIdx);

const apiReference = buildApiReference(apiData.modules);

const output = before + '\n\n' + apiReference + '\n' + after;

const outPath = join(siteRoot, 'static', 'llms-full.txt');
writeFileSync(outPath, output, 'utf-8');

const moduleCount = apiData.modules.length;
const fnCount = apiData.modules.reduce((sum, m) => sum + m.functions.length, 0);
console.log(`Generated ${outPath}`);
console.log(`  ${moduleCount} modules, ${fnCount} functions`);
console.log(`  ${output.length} bytes`);
