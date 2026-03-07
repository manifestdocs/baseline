<svelte:head>
    <title>Error Handling | Baseline Examples</title>
    <meta
        name="description"
        content="Error handling in Baseline with Result, Option, the ? operator, and composing fallible operations."
    />
    <meta property="og:title" content="Error Handling | Baseline Examples" />
    <meta property="og:description" content="Error handling in Baseline with Result, Option, the ? operator, and composing fallible operations." />
    <meta name="twitter:title" content="Error Handling | Baseline Examples" />
    <meta name="twitter:description" content="Error handling in Baseline with Result, Option, the ? operator, and composing fallible operations." />
</svelte:head>

<main id="main-content" class="pt-[var(--site-top)] pr-8 pb-20 pl-[var(--content-left)]">
    <div class="grid grid-cols-[1fr_14rem] gap-[var(--content-left)] items-start">
        <article class="interior-content guide-content max-w-[48em] min-w-0">
            <h1 class="interior-h1 mb-5">Error Handling</h1>
            <p class="interior-intro max-w-[44rem] mb-5">
                Baseline has no exceptions, no try/catch, no null.
                Errors are values: <code>Result&lt;T, E&gt;</code> for
                operations that can fail, <code>Option&lt;T&gt;</code> for
                values that might be absent. The <code>?</code> operator
                propagates errors up the call chain.
            </p>

            <section id="result-basics">
                <h2>Result Basics</h2>
                <p>
                    A function that can fail returns <code>Result&lt;T, E&gt;</code>.
                    The caller must handle both <code>Ok</code> and
                    <code>Err</code> cases.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">script</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">safe_divide</span><span class="punct">(</span><span class="fn">a</span><span class="op">:</span> <span class="type">Int</span><span class="punct">,</span> <span class="fn">b</span><span class="op">:</span> <span class="type">Int</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Result&lt;Int, String&gt;</span> <span class="op">=</span>
  <span class="kw">if</span> <span class="fn">b</span> <span class="op">==</span> <span class="num">0</span> <span class="kw">then</span> <span class="type">Err</span><span class="punct">(</span><span class="str">"Division by zero"</span><span class="punct">)</span>
  <span class="kw">else</span> <span class="type">Ok</span><span class="punct">(</span><span class="fn">a</span> <span class="op">/</span> <span class="fn">b</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">validate_age</span><span class="punct">(</span><span class="fn">age</span><span class="op">:</span> <span class="type">Int</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Result&lt;Int, String&gt;</span> <span class="op">=</span>
  <span class="kw">if</span> <span class="fn">age</span> <span class="op">&lt;</span> <span class="num">0</span> <span class="kw">then</span> <span class="type">Err</span><span class="punct">(</span><span class="str">"Age cannot be negative"</span><span class="punct">)</span>
  <span class="kw">else</span> <span class="kw">if</span> <span class="fn">age</span> <span class="op">&gt;</span> <span class="num">150</span> <span class="kw">then</span> <span class="type">Err</span><span class="punct">(</span><span class="str">"Age seems unrealistic"</span><span class="punct">)</span>
  <span class="kw">else</span> <span class="type">Ok</span><span class="punct">(</span><span class="fn">age</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">main!</span><span class="punct">()</span> <span class="op">-&gt;</span> <span class="effect">{Console}</span> <span class="punct">()</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">match</span> <span class="fn">safe_divide</span><span class="punct">(</span><span class="num">10</span><span class="punct">,</span> <span class="num">3</span><span class="punct">)</span>
    <span class="type">Ok</span><span class="punct">(</span><span class="fn">result</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"10 / 3 = \${result}"</span><span class="punct">)</span>
    <span class="type">Err</span><span class="punct">(</span><span class="fn">msg</span><span class="punct">)</span>   <span class="op">-&gt;</span> <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"Error: \${msg}"</span><span class="punct">)</span>
  <span class="kw">match</span> <span class="fn">safe_divide</span><span class="punct">(</span><span class="num">10</span><span class="punct">,</span> <span class="num">0</span><span class="punct">)</span>
    <span class="type">Ok</span><span class="punct">(</span><span class="fn">result</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"10 / 0 = \${result}"</span><span class="punct">)</span>
    <span class="type">Err</span><span class="punct">(</span><span class="fn">msg</span><span class="punct">)</span>   <span class="op">-&gt;</span> <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"Error: \${msg}"</span><span class="punct">)</span>
<span class="punct">}</span>`}</code></pre>
            </section>

            <section id="question-mark">
                <h2>The ? Operator</h2>
                <p>
                    The <code>?</code> operator propagates errors. If the value
                    is <code>Err</code>, it returns early from the function.
                    If <code>Ok</code>, it unwraps the value and continues.
                    This keeps error handling concise without hiding it.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">script</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">parse_int</span><span class="punct">(</span><span class="fn">s</span><span class="op">:</span> <span class="type">String</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Result&lt;Int, String&gt;</span> <span class="op">=</span>
  <span class="type">Int</span><span class="punct">.</span><span class="fn">parse</span><span class="punct">(</span><span class="fn">s</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">parse_and_add</span><span class="punct">(</span><span class="fn">a</span><span class="op">:</span> <span class="type">String</span><span class="punct">,</span> <span class="fn">b</span><span class="op">:</span> <span class="type">String</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Result&lt;Int, String&gt;</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">x</span> <span class="op">=</span> <span class="fn">parse_int</span><span class="punct">(</span><span class="fn">a</span><span class="punct">)</span><span class="op">?</span>
  <span class="kw">let</span> <span class="fn">y</span> <span class="op">=</span> <span class="fn">parse_int</span><span class="punct">(</span><span class="fn">b</span><span class="punct">)</span><span class="op">?</span>
  <span class="type">Ok</span><span class="punct">(</span><span class="fn">x</span> <span class="op">+</span> <span class="fn">y</span><span class="punct">)</span>
<span class="punct">}</span>

<span class="kw">fn</span> <span class="fn">compute_ratio</span><span class="punct">(</span><span class="fn">num</span><span class="op">:</span> <span class="type">String</span><span class="punct">,</span> <span class="fn">den</span><span class="op">:</span> <span class="type">String</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Result&lt;Int, String&gt;</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">n</span> <span class="op">=</span> <span class="fn">parse_int</span><span class="punct">(</span><span class="fn">num</span><span class="punct">)</span><span class="op">?</span>
  <span class="kw">let</span> <span class="fn">d</span> <span class="op">=</span> <span class="fn">parse_int</span><span class="punct">(</span><span class="fn">den</span><span class="punct">)</span><span class="op">?</span>
  <span class="kw">if</span> <span class="fn">d</span> <span class="op">==</span> <span class="num">0</span> <span class="kw">then</span> <span class="type">Err</span><span class="punct">(</span><span class="str">"Cannot divide by zero"</span><span class="punct">)</span>
  <span class="kw">else</span> <span class="type">Ok</span><span class="punct">(</span><span class="fn">n</span> <span class="op">/</span> <span class="fn">d</span><span class="punct">)</span>
<span class="punct">}</span>

<span class="kw">fn</span> <span class="fn">main!</span><span class="punct">()</span> <span class="op">-&gt;</span> <span class="effect">{Console}</span> <span class="punct">()</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"parse_and_add: \${parse_and_add(<span class="str">"3"</span>, <span class="str">"4"</span>)}"</span><span class="punct">)</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"bad input:    \${parse_and_add(<span class="str">"3"</span>, <span class="str">"abc"</span>)}"</span><span class="punct">)</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"ratio:        \${compute_ratio(<span class="str">"100"</span>, <span class="str">"3"</span>)}"</span><span class="punct">)</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"div by zero:  \${compute_ratio(<span class="str">"100"</span>, <span class="str">"0"</span>)}"</span><span class="punct">)</span>
<span class="punct">}</span>`}</code></pre>
            </section>

            <section id="chaining">
                <h2>Chaining Fallible Operations</h2>
                <p>
                    Combine <code>?</code> with pipes and pattern matching
                    to build multi-step validation pipelines where each step
                    can fail independently.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">script</span><span class="punct">)</span>

<span class="kw">type</span> <span class="type">User</span> <span class="op">=</span> <span class="punct">{</span> <span class="fn">name</span><span class="op">:</span> <span class="type">String</span><span class="punct">,</span> <span class="fn">email</span><span class="op">:</span> <span class="type">String</span><span class="punct">,</span> <span class="fn">age</span><span class="op">:</span> <span class="type">Int</span> <span class="punct">}</span>

<span class="kw">fn</span> <span class="fn">validate_name</span><span class="punct">(</span><span class="fn">name</span><span class="op">:</span> <span class="type">String</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Result&lt;String, String&gt;</span> <span class="op">=</span>
  <span class="kw">if</span> <span class="type">String</span><span class="punct">.</span><span class="fn">length</span><span class="punct">(</span><span class="fn">name</span><span class="punct">)</span> <span class="op">==</span> <span class="num">0</span> <span class="kw">then</span> <span class="type">Err</span><span class="punct">(</span><span class="str">"Name cannot be empty"</span><span class="punct">)</span>
  <span class="kw">else</span> <span class="type">Ok</span><span class="punct">(</span><span class="fn">name</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">validate_email</span><span class="punct">(</span><span class="fn">email</span><span class="op">:</span> <span class="type">String</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Result&lt;String, String&gt;</span> <span class="op">=</span>
  <span class="kw">if</span> <span class="type">String</span><span class="punct">.</span><span class="fn">contains</span><span class="punct">(</span><span class="fn">email</span><span class="punct">,</span> <span class="str">"@"</span><span class="punct">)</span> <span class="kw">then</span> <span class="type">Ok</span><span class="punct">(</span><span class="fn">email</span><span class="punct">)</span>
  <span class="kw">else</span> <span class="type">Err</span><span class="punct">(</span><span class="str">"Invalid email: missing @"</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">validate_age</span><span class="punct">(</span><span class="fn">age</span><span class="op">:</span> <span class="type">Int</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Result&lt;Int, String&gt;</span> <span class="op">=</span>
  <span class="kw">if</span> <span class="fn">age</span> <span class="op">&lt;</span> <span class="num">0</span> <span class="kw">then</span> <span class="type">Err</span><span class="punct">(</span><span class="str">"Age cannot be negative"</span><span class="punct">)</span>
  <span class="kw">else</span> <span class="kw">if</span> <span class="fn">age</span> <span class="op">&gt;</span> <span class="num">150</span> <span class="kw">then</span> <span class="type">Err</span><span class="punct">(</span><span class="str">"Age seems unrealistic"</span><span class="punct">)</span>
  <span class="kw">else</span> <span class="type">Ok</span><span class="punct">(</span><span class="fn">age</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">create_user</span><span class="punct">(</span><span class="fn">name</span><span class="op">:</span> <span class="type">String</span><span class="punct">,</span> <span class="fn">email</span><span class="op">:</span> <span class="type">String</span><span class="punct">,</span> <span class="fn">age</span><span class="op">:</span> <span class="type">Int</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Result&lt;User, String&gt;</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">valid_name</span> <span class="op">=</span> <span class="fn">validate_name</span><span class="punct">(</span><span class="fn">name</span><span class="punct">)</span><span class="op">?</span>
  <span class="kw">let</span> <span class="fn">valid_email</span> <span class="op">=</span> <span class="fn">validate_email</span><span class="punct">(</span><span class="fn">email</span><span class="punct">)</span><span class="op">?</span>
  <span class="kw">let</span> <span class="fn">valid_age</span> <span class="op">=</span> <span class="fn">validate_age</span><span class="punct">(</span><span class="fn">age</span><span class="punct">)</span><span class="op">?</span>
  <span class="type">Ok</span><span class="punct">(</span><span class="type">User</span> <span class="punct">{</span> <span class="fn">name</span><span class="op">:</span> <span class="fn">valid_name</span><span class="punct">,</span> <span class="fn">email</span><span class="op">:</span> <span class="fn">valid_email</span><span class="punct">,</span> <span class="fn">age</span><span class="op">:</span> <span class="fn">valid_age</span> <span class="punct">})</span>
<span class="punct">}</span>

<span class="kw">fn</span> <span class="fn">main!</span><span class="punct">()</span> <span class="op">-&gt;</span> <span class="effect">{Console}</span> <span class="punct">()</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">match</span> <span class="fn">create_user</span><span class="punct">(</span><span class="str">"Alice"</span><span class="punct">,</span> <span class="str">"alice@example.com"</span><span class="punct">,</span> <span class="num">30</span><span class="punct">)</span>
    <span class="type">Ok</span><span class="punct">(</span><span class="fn">user</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"Created: \${user.name}"</span><span class="punct">)</span>
    <span class="type">Err</span><span class="punct">(</span><span class="fn">msg</span><span class="punct">)</span>  <span class="op">-&gt;</span> <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"Failed: \${msg}"</span><span class="punct">)</span>
  <span class="kw">match</span> <span class="fn">create_user</span><span class="punct">(</span><span class="str">""</span><span class="punct">,</span> <span class="str">"bad"</span><span class="punct">,</span> <span class="num">-1</span><span class="punct">)</span>
    <span class="type">Ok</span><span class="punct">(</span><span class="fn">user</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"Created: \${user.name}"</span><span class="punct">)</span>
    <span class="type">Err</span><span class="punct">(</span><span class="fn">msg</span><span class="punct">)</span>  <span class="op">-&gt;</span> <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"Failed: \${msg}"</span><span class="punct">)</span>
<span class="punct">}</span>`}</code></pre>
            </section>

            <p class="mt-12 pt-5 border-t border-[var(--fg-faint)]">
                <a href="/examples" class="text-sm text-[var(--fg-dim)] no-underline hover:text-[var(--fg)]">&larr; All Examples</a>
            </p>
        </article>

        <aside class="toc-sidebar sticky top-[var(--site-top)] max-h-[calc(100vh-3.25rem)] overflow-y-auto pb-8" aria-label="Example sections">
            <h2 class="toc-title mb-3">On this page</h2>
            <nav>
                <ul role="list">
                    <li><a href="#result-basics">Result Basics</a></li>
                    <li><a href="#question-mark">The ? Operator</a></li>
                    <li><a href="#chaining">Chaining Fallible Operations</a></li>
                </ul>
            </nav>
        </aside>
    </div>
</main>
