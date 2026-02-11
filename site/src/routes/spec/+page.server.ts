import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { parseSpec } from '$lib/utils/markdown';

export const prerender = true;

export function load() {
	const specPath = resolve('..', 'design', 'baseline-language-specification.md');
	const markdown = readFileSync(specPath, 'utf-8');
	const { html, toc } = parseSpec(markdown);

	return { html, toc };
}
