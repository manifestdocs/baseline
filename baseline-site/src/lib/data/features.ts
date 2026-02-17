export interface Feature {
	title: string;
	description: string;
}

export const features: Feature[] = [
	{
		title: 'Algebraic Effects',
		description:
			'Side effects declared in type signatures and enforced by the compiler. Built-in: Console, Http, Fs, Log, Time, Random, Env, and more.'
	},
	{
		title: 'Refinement Types',
		description:
			'<code>Int where self &gt; 0</code> proves constraints at compile time. Parse, don\'t validate.'
	},
	{
		title: 'Pattern Matching',
		description:
			'Exhaustive matches on sum types and tuples with constructor destructuring. The compiler rejects unhandled branches.'
	},
	{
		title: 'Pipe Operator',
		description:
			'<code>|&gt;</code> chains data transformations left to right. The compiler warns against deep nesting.'
	},
	{
		title: 'Sum Types',
		description:
			'Model domain variants precisely. <code>Option&lt;T&gt;</code> and <code>Result&lt;T, E&gt;</code> are built-in.'
	},
	{
		title: 'Records',
		description: 'Named fields, dot access, update syntax, structural typing.'
	},
	{
		title: 'Inline Tests',
		description:
			'<code>@test</code> sections keep tests alongside the code they verify, with clear separation from production code.'
	},
	{
		title: 'Functions',
		description:
			'<code>fn name(args: Type) -> ReturnType = body</code>. Lambdas with <code>|x| expr</code>. Type inference for locals.'
	},
	{
		title: 'Modules',
		description:
			'<code>@module Name</code> with explicit imports. Prelude levels from none to server.'
	},
	{
		title: 'Error Propagation',
		description:
			'The <code>?</code> operator propagates errors. <code>match</code> for explicit handling. No try/catch.'
	},
	{
		title: 'Structured Diagnostics',
		description:
			'Compiler errors come as structured JSON with source locations, suggestions, and fix hints. Useful for editor tooling and AI agents alike.'
	},
	{
		title: 'AI-Friendly',
		description:
			'Unambiguous syntax and structured error output make Baseline a natural fit for AI code generation and compiler-driven self-correction.'
	}
];
