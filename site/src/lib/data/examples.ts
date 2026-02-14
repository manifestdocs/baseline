export interface CodeExample {
	title: string;
	description: string;
	code: string;
}

export const examples: CodeExample[] = [
	{
		title: 'Pattern matching',
		description:
			'Exhaustive by default. The compiler rejects unhandled branches. Model your domain with sum types and let the type system ensure every case is covered.',
		code: `<span class="kw">type</span> <span class="type">Connection</span> <span class="op">=</span>
  <span class="op">|</span> <span class="type">Disconnected</span>
  <span class="op">|</span> <span class="type">Connected</span><span class="punct">(</span><span class="type">Socket</span><span class="punct">)</span>
  <span class="op">|</span> <span class="type">Error</span><span class="punct">(</span><span class="type">String</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">handle</span><span class="punct">(</span><span class="fn">conn</span><span class="op">:</span> <span class="type">Connection</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">String</span> <span class="op">=</span>
  <span class="kw">match</span> <span class="fn">conn</span>
    <span class="type">Disconnected</span>      <span class="op">-&gt;</span> <span class="str">"reconnecting"</span>
    <span class="type">Connected</span><span class="punct">(</span><span class="fn">socket</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="str">"active"</span>
    <span class="type">Error</span><span class="punct">(</span><span class="fn">msg</span><span class="punct">)</span>       <span class="op">-&gt;</span> <span class="str">"failed: \${msg}"</span>`
	},
	{
		title: 'Pipes and transforms',
		description:
			'The pipe operator <code>|&gt;</code> chains transformations left to right. Data flow reads like prose. Combinators replace loops for filtering, mapping, and folding.',
		code: `<span class="kw">fn</span> <span class="fn">active_users</span><span class="punct">(</span><span class="fn">users</span><span class="op">:</span> <span class="type">List&lt;User&gt;</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">List&lt;String&gt;</span> <span class="op">=</span>
  <span class="fn">users</span>
  <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">filter</span><span class="punct">(|</span><span class="fn">u</span><span class="op">|</span> <span class="fn">u</span><span class="punct">.</span><span class="fn">active</span><span class="punct">)</span>
  <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">map</span><span class="punct">(|</span><span class="fn">u</span><span class="op">|</span> <span class="fn">u</span><span class="punct">.</span><span class="fn">name</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">total_age</span><span class="punct">(</span><span class="fn">users</span><span class="op">:</span> <span class="type">List&lt;User&gt;</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Int</span> <span class="op">=</span>
  <span class="fn">users</span>
  <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">map</span><span class="punct">(|</span><span class="fn">u</span><span class="op">|</span> <span class="fn">u</span><span class="punct">.</span><span class="fn">age</span><span class="punct">)</span>
  <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">fold</span><span class="punct">(</span><span class="num">0</span><span class="punct">,</span> <span class="op">|</span><span class="fn">acc</span><span class="punct">,</span> <span class="fn">x</span><span class="op">|</span> <span class="fn">acc</span> <span class="op">+</span> <span class="fn">x</span><span class="punct">)</span>`
	},
	{
		title: 'Tracked effects',
		description:
			'Every side effect is declared in the type signature. <code>{Http, Console}</code> means this function can fetch data and print, and nothing else. The <code>?</code> operator propagates errors.',
		code: `<span class="kw">fn</span> <span class="fn">fetch_user!</span><span class="punct">(</span><span class="fn">id</span><span class="op">:</span> <span class="type">Int</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="effect">{Http, Console}</span> <span class="type">Result&lt;String, String&gt;</span> <span class="op">=</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">print!</span><span class="punct">(</span><span class="str">"Fetching user \${id}"</span><span class="punct">)</span>
  <span class="kw">let</span> <span class="fn">response</span> <span class="op">=</span> <span class="type">Http</span><span class="punct">.</span><span class="fn">get!</span><span class="punct">(</span><span class="str">"/users/\${id}"</span><span class="punct">)</span><span class="op">?</span>
  <span class="kw">match</span> <span class="fn">response</span><span class="punct">.</span><span class="fn">status</span>
    <span class="num">200</span> <span class="op">-&gt;</span> <span class="type">Ok</span><span class="punct">(</span><span class="fn">response</span><span class="punct">.</span><span class="fn">body</span><span class="punct">)</span>
    <span class="num">404</span> <span class="op">-&gt;</span> <span class="type">Err</span><span class="punct">(</span><span class="str">"User not found"</span><span class="punct">)</span>
    <span class="fn">code</span> <span class="op">-&gt;</span> <span class="type">Err</span><span class="punct">(</span><span class="str">"HTTP \${code}"</span><span class="punct">)</span>`
	},
	{
		title: 'Inline tests',
		description:
			'Tests live in <code>@test</code> sections, separate from production code. Functions and their tests share the same file and scope, keeping them always in sync.',
		code: `<span class="kw">fn</span> <span class="fn">add</span><span class="punct">(</span><span class="fn">a</span><span class="op">:</span> <span class="type">Int</span><span class="punct">,</span> <span class="fn">b</span><span class="op">:</span> <span class="type">Int</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Int</span> <span class="op">=</span> <span class="fn">a</span> <span class="op">+</span> <span class="fn">b</span>

<span class="kw">fn</span> <span class="fn">safe_divide</span><span class="punct">(</span><span class="fn">a</span><span class="op">:</span> <span class="type">Int</span><span class="punct">,</span> <span class="fn">b</span><span class="op">:</span> <span class="type">Int</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Result&lt;Int, String&gt;</span> <span class="op">=</span>
  <span class="kw">if</span> <span class="fn">b</span> <span class="op">==</span> <span class="num">0</span> <span class="kw">then</span> <span class="type">Err</span><span class="punct">(</span><span class="str">"Division by zero"</span><span class="punct">)</span>
  <span class="kw">else</span> <span class="type">Ok</span><span class="punct">(</span><span class="fn">a</span> <span class="op">/</span> <span class="fn">b</span><span class="punct">)</span>

<span class="attr">@test</span>
<span class="kw">test</span> <span class="str">"positive"</span>  <span class="op">=</span> <span class="fn">add</span><span class="punct">(</span><span class="num">1</span><span class="punct">,</span> <span class="num">2</span><span class="punct">)</span> <span class="op">==</span> <span class="num">3</span>
<span class="kw">test</span> <span class="str">"zero"</span>      <span class="op">=</span> <span class="fn">add</span><span class="punct">(</span><span class="num">0</span><span class="punct">,</span> <span class="num">5</span><span class="punct">)</span> <span class="op">==</span> <span class="num">5</span>
<span class="kw">test</span> <span class="str">"divides"</span>   <span class="op">=</span> <span class="fn">safe_divide</span><span class="punct">(</span><span class="num">10</span><span class="punct">,</span> <span class="num">2</span><span class="punct">)</span> <span class="op">==</span> <span class="type">Ok</span><span class="punct">(</span><span class="num">5</span><span class="punct">)</span>
<span class="kw">test</span> <span class="str">"by zero"</span>   <span class="op">=</span> <span class="fn">safe_divide</span><span class="punct">(</span><span class="num">1</span><span class="punct">,</span> <span class="num">0</span><span class="punct">)</span> <span class="op">==</span> <span class="type">Err</span><span class="punct">(</span><span class="str">"Division by zero"</span><span class="punct">)</span>`
	}
];
