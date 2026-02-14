import { Marked } from 'marked';
import { gfmHeadingId } from 'marked-gfm-heading-id';

export interface TocEntry {
	id: string;
	text: string;
	level: number;
}

export interface ParsedSpec {
	html: string;
	toc: TocEntry[];
}

const BASELINE_KEYWORDS = new Set([
	'fn', 'type', 'let', 'match', 'if', 'else', 'then', 'where', 'test',
	'import', 'module', 'not', 'true', 'false', 'return', 'with', 'from',
	'as', 'for', 'in', 'do', 'end', 'and', 'or', 'try', 'handle', 'effect',
	'struct', 'enum', 'alias', 'given', 'when', 'expect', 'is'
]);

const BASELINE_TYPES = new Set([
	'Int', 'String', 'Boolean', 'Float', 'List', 'Option', 'Result',
	'Unit', 'None', 'Some', 'Ok', 'Err', 'Map', 'Set', 'Tuple',
	'Void', 'Never', 'Any', 'Self', 'Bool'
]);

function highlightBaseline(code: string): string {
	// Escape HTML first
	let escaped = code
		.replace(/&/g, '&amp;')
		.replace(/</g, '&lt;')
		.replace(/>/g, '&gt;');

	const tokens: { start: number; end: number; html: string }[] = [];

	// Comments (must be first — content inside should not be re-highlighted)
	const commentRe = /\/\/[^\n]*/g;
	let m;
	while ((m = commentRe.exec(escaped)) !== null) {
		tokens.push({ start: m.index, end: m.index + m[0].length, html: `<span class="comment">${m[0]}</span>` });
	}

	// Strings
	const stringRe = /"(?:[^"\\]|\\.)*"/g;
	while ((m = stringRe.exec(escaped)) !== null) {
		tokens.push({ start: m.index, end: m.index + m[0].length, html: `<span class="str">${m[0]}</span>` });
	}

	// Effects in signatures: {Console, Http}
	const effectRe = /\{([A-Z]\w*(?:\s*,\s*[A-Z]\w*)*)\}/g;
	while ((m = effectRe.exec(escaped)) !== null) {
		tokens.push({ start: m.index, end: m.index + m[0].length, html: `<span class="effect">${m[0]}</span>` });
	}

	// Annotations: @module, @prelude, etc.
	const annotRe = /@\w+/g;
	while ((m = annotRe.exec(escaped)) !== null) {
		tokens.push({ start: m.index, end: m.index + m[0].length, html: `<span class="annot">${m[0]}</span>` });
	}

	// Numbers
	const numRe = /\b\d+(\.\d+)?\b/g;
	while ((m = numRe.exec(escaped)) !== null) {
		tokens.push({ start: m.index, end: m.index + m[0].length, html: `<span class="num">${m[0]}</span>` });
	}

	// Operators (multi-char first)
	const opRe = /->|&lt;-|\|&gt;|==|!=|&lt;=|&gt;=|&lt;|&gt;|\?\.|\.\.|\+|-|\*|\/|=|\||\?|::/g;
	while ((m = opRe.exec(escaped)) !== null) {
		tokens.push({ start: m.index, end: m.index + m[0].length, html: `<span class="op">${m[0]}</span>` });
	}

	// Words — keywords, types, function names
	const wordRe = /\b[A-Za-z_]\w*!?\b/g;
	while ((m = wordRe.exec(escaped)) !== null) {
		const word = m[0];
		if (BASELINE_KEYWORDS.has(word)) {
			tokens.push({ start: m.index, end: m.index + word.length, html: `<span class="kw">${word}</span>` });
		} else if (BASELINE_TYPES.has(word)) {
			tokens.push({ start: m.index, end: m.index + word.length, html: `<span class="type">${word}</span>` });
		} else if (word.endsWith('!')) {
			tokens.push({ start: m.index, end: m.index + word.length, html: `<span class="fn">${word}</span>` });
		}
	}

	// Sort by start position, remove overlaps (earlier tokens win)
	tokens.sort((a, b) => a.start - b.start);
	const merged: typeof tokens = [];
	let lastEnd = 0;
	for (const t of tokens) {
		if (t.start >= lastEnd) {
			merged.push(t);
			lastEnd = t.end;
		}
	}

	// Reconstruct string
	let result = '';
	let pos = 0;
	for (const t of merged) {
		result += escaped.slice(pos, t.start);
		result += t.html;
		pos = t.end;
	}
	result += escaped.slice(pos);

	return result;
}

export function parseSpec(markdown: string): ParsedSpec {
	const toc: TocEntry[] = [];

	const marked = new Marked();
	marked.use(gfmHeadingId());

	marked.use({
		renderer: {
			heading({ tokens, depth }) {
				const text = tokens.map(t => ('text' in t ? t.text : t.raw)).join('');
				// Strip markdown formatting for clean text
				const cleanText = text.replace(/[`*_]/g, '');
				const id = cleanText
					.toLowerCase()
					.replace(/[^\w\s-]/g, '')
					.replace(/\s+/g, '-')
					.replace(/-+/g, '-')
					.replace(/^-|-$/g, '');

				if (depth >= 2 && depth <= 3) {
					toc.push({ id, text: cleanText, level: depth });
				}

				return `<h${depth} id="${id}">${this.parser.parseInline(tokens)}</h${depth}>`;
			},

			code({ text, lang }) {
				const language = lang || '';
				const isBaseline = language === 'baseline' || language === 'bl' || language === '';
				const highlighted = isBaseline ? highlightBaseline(text) : escapeHtml(text);
				const langClass = language ? ` data-lang="${language}"` : '';
				return `<pre${langClass}><code>${highlighted}</code></pre>`;
			},

			link({ href, tokens }) {
				const text = this.parser.parseInline(tokens);
				// Rewrite relative .md links to GitHub
				if (href && href.endsWith('.md') && !href.startsWith('http')) {
					href = `https://github.com/manifestdocs/baseline/blob/main/design/${href}`;
				}
				const external = href && href.startsWith('http') ? ' target="_blank" rel="noopener"' : '';
				return `<a href="${href}"${external}>${text}</a>`;
			},

			table({ header, rows }) {
				const thead = '<thead><tr>' +
					header.map(cell => {
						const align = cell.align ? ` style="text-align:${cell.align}"` : '';
						return `<th${align}>${this.parser.parseInline(cell.tokens)}</th>`;
					}).join('') +
					'</tr></thead>';

				const tbody = rows.length > 0
					? '<tbody>' + rows.map(row =>
						'<tr>' + row.map(cell => {
							const align = cell.align ? ` style="text-align:${cell.align}"` : '';
							return `<td${align}>${this.parser.parseInline(cell.tokens)}</td>`;
						}).join('') + '</tr>'
					).join('') + '</tbody>'
					: '';

				return `<div class="table-wrap"><table>${thead}${tbody}</table></div>`;
			}
		}
	});

	const html = marked.parse(markdown) as string;

	return { html, toc };
}

function escapeHtml(text: string): string {
	return text
		.replace(/&/g, '&amp;')
		.replace(/</g, '&lt;')
		.replace(/>/g, '&gt;');
}
