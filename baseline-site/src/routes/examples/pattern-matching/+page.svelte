<svelte:head>
    <title>Pattern Matching | Baseline Examples</title>
    <meta
        name="description"
        content="Sum types and exhaustive pattern matching in Baseline: model your domain precisely and let the compiler verify every case."
    />
    <meta property="og:title" content="Pattern Matching | Baseline Examples" />
    <meta property="og:description" content="Sum types and exhaustive pattern matching in Baseline." />
    <meta name="twitter:title" content="Pattern Matching | Baseline Examples" />
    <meta name="twitter:description" content="Sum types and exhaustive pattern matching in Baseline." />
</svelte:head>

<main id="main-content" class="pt-[var(--site-top)] pr-8 pb-20 pl-[var(--content-left)]">
    <div class="grid grid-cols-[1fr_14rem] gap-[var(--content-left)] items-start">
        <article class="interior-content guide-content max-w-[48em] min-w-0">
            <h1 class="interior-h1 mb-5">Pattern Matching</h1>
            <p class="interior-intro max-w-[44rem] mb-5">
                Sum types model domain variants precisely. Pattern matching
                destructures them exhaustively: the compiler rejects programs
                that leave a case unhandled.
            </p>

            <section id="basic-sum-types">
                <h2>Basic Sum Types</h2>
                <p>
                    Define a type with variants using <code>|</code>. Each variant
                    can carry data. Match on every case to extract values.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">script</span><span class="punct">)</span>

<span class="kw">type</span> <span class="type">Shape</span> <span class="op">=</span>
  <span class="op">|</span> <span class="type">Circle</span><span class="punct">(</span><span class="type">Int</span><span class="punct">)</span>
  <span class="op">|</span> <span class="type">Rectangle</span><span class="punct">(</span><span class="type">Int</span><span class="punct">,</span> <span class="type">Int</span><span class="punct">)</span>
  <span class="op">|</span> <span class="type">Triangle</span><span class="punct">(</span><span class="type">Int</span><span class="punct">,</span> <span class="type">Int</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">area</span><span class="punct">(</span><span class="fn">shape</span><span class="op">:</span> <span class="type">Shape</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Int</span> <span class="op">=</span>
  <span class="kw">match</span> <span class="fn">shape</span>
    <span class="type">Circle</span><span class="punct">(</span><span class="fn">r</span><span class="punct">)</span>          <span class="op">-&gt;</span> <span class="num">3</span> <span class="op">*</span> <span class="fn">r</span> <span class="op">*</span> <span class="fn">r</span>
    <span class="type">Rectangle</span><span class="punct">(</span><span class="fn">w</span><span class="punct">,</span> <span class="fn">h</span><span class="punct">)</span>   <span class="op">-&gt;</span> <span class="fn">w</span> <span class="op">*</span> <span class="fn">h</span>
    <span class="type">Triangle</span><span class="punct">(</span><span class="fn">b</span><span class="punct">,</span> <span class="fn">h</span><span class="punct">)</span>    <span class="op">-&gt;</span> <span class="fn">b</span> <span class="op">*</span> <span class="fn">h</span> <span class="op">/</span> <span class="num">2</span>

<span class="kw">fn</span> <span class="fn">describe</span><span class="punct">(</span><span class="fn">shape</span><span class="op">:</span> <span class="type">Shape</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">String</span> <span class="op">=</span>
  <span class="kw">match</span> <span class="fn">shape</span>
    <span class="type">Circle</span><span class="punct">(</span><span class="fn">r</span><span class="punct">)</span>          <span class="op">-&gt;</span> <span class="str">"circle with radius \${r}"</span>
    <span class="type">Rectangle</span><span class="punct">(</span><span class="fn">w</span><span class="punct">,</span> <span class="fn">h</span><span class="punct">)</span>   <span class="op">-&gt;</span> <span class="str">"\${w}x\${h} rectangle"</span>
    <span class="type">Triangle</span><span class="punct">(</span><span class="fn">b</span><span class="punct">,</span> <span class="fn">h</span><span class="punct">)</span>    <span class="op">-&gt;</span> <span class="str">"triangle (base \${b}, height \${h})"</span>

<span class="kw">fn</span> <span class="fn">main!</span><span class="punct">()</span> <span class="op">-&gt;</span> <span class="effect">{Console}</span> <span class="punct">()</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">shapes</span> <span class="op">=</span> <span class="punct">[</span><span class="type">Circle</span><span class="punct">(</span><span class="num">5</span><span class="punct">),</span> <span class="type">Rectangle</span><span class="punct">(</span><span class="num">4</span><span class="punct">,</span> <span class="num">6</span><span class="punct">),</span> <span class="type">Triangle</span><span class="punct">(</span><span class="num">3</span><span class="punct">,</span> <span class="num">8</span><span class="punct">)]</span>
  <span class="fn">shapes</span>
  <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">map</span><span class="punct">(|</span><span class="fn">s</span><span class="punct">|</span> <span class="str">"\${describe(s)}: area = \${area(s)}"</span><span class="punct">)</span>
  <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">map</span><span class="punct">(|</span><span class="fn">line</span><span class="punct">|</span> <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="fn">line</span><span class="punct">))</span>
<span class="punct">}</span>`}</code></pre>
            </section>

            <section id="nested-matching">
                <h2>Nested and Complex Patterns</h2>
                <p>
                    Patterns can be nested. Use <code>Option</code> and
                    <code>Result</code> to handle missing or fallible values.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">script</span><span class="punct">)</span>

<span class="kw">type</span> <span class="type">Expr</span> <span class="op">=</span>
  <span class="op">|</span> <span class="type">Num</span><span class="punct">(</span><span class="type">Int</span><span class="punct">)</span>
  <span class="op">|</span> <span class="type">Add</span><span class="punct">(</span><span class="type">Expr</span><span class="punct">,</span> <span class="type">Expr</span><span class="punct">)</span>
  <span class="op">|</span> <span class="type">Mul</span><span class="punct">(</span><span class="type">Expr</span><span class="punct">,</span> <span class="type">Expr</span><span class="punct">)</span>
  <span class="op">|</span> <span class="type">Neg</span><span class="punct">(</span><span class="type">Expr</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">eval</span><span class="punct">(</span><span class="fn">expr</span><span class="op">:</span> <span class="type">Expr</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Int</span> <span class="op">=</span>
  <span class="kw">match</span> <span class="fn">expr</span>
    <span class="type">Num</span><span class="punct">(</span><span class="fn">n</span><span class="punct">)</span>       <span class="op">-&gt;</span> <span class="fn">n</span>
    <span class="type">Add</span><span class="punct">(</span><span class="fn">a</span><span class="punct">,</span> <span class="fn">b</span><span class="punct">)</span>   <span class="op">-&gt;</span> <span class="fn">eval</span><span class="punct">(</span><span class="fn">a</span><span class="punct">)</span> <span class="op">+</span> <span class="fn">eval</span><span class="punct">(</span><span class="fn">b</span><span class="punct">)</span>
    <span class="type">Mul</span><span class="punct">(</span><span class="fn">a</span><span class="punct">,</span> <span class="fn">b</span><span class="punct">)</span>   <span class="op">-&gt;</span> <span class="fn">eval</span><span class="punct">(</span><span class="fn">a</span><span class="punct">)</span> <span class="op">*</span> <span class="fn">eval</span><span class="punct">(</span><span class="fn">b</span><span class="punct">)</span>
    <span class="type">Neg</span><span class="punct">(</span><span class="fn">e</span><span class="punct">)</span>       <span class="op">-&gt;</span> <span class="num">0</span> <span class="op">-</span> <span class="fn">eval</span><span class="punct">(</span><span class="fn">e</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">show</span><span class="punct">(</span><span class="fn">expr</span><span class="op">:</span> <span class="type">Expr</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">String</span> <span class="op">=</span>
  <span class="kw">match</span> <span class="fn">expr</span>
    <span class="type">Num</span><span class="punct">(</span><span class="fn">n</span><span class="punct">)</span>       <span class="op">-&gt;</span> <span class="str">"\${n}"</span>
    <span class="type">Add</span><span class="punct">(</span><span class="fn">a</span><span class="punct">,</span> <span class="fn">b</span><span class="punct">)</span>   <span class="op">-&gt;</span> <span class="str">"(\${show(a)} + \${show(b)})"</span>
    <span class="type">Mul</span><span class="punct">(</span><span class="fn">a</span><span class="punct">,</span> <span class="fn">b</span><span class="punct">)</span>   <span class="op">-&gt;</span> <span class="str">"(\${show(a)} * \${show(b)})"</span>
    <span class="type">Neg</span><span class="punct">(</span><span class="fn">e</span><span class="punct">)</span>       <span class="op">-&gt;</span> <span class="str">"(-\${show(e)})"</span>

<span class="kw">fn</span> <span class="fn">main!</span><span class="punct">()</span> <span class="op">-&gt;</span> <span class="effect">{Console}</span> <span class="punct">()</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="comment">// (2 + 3) * -(4)</span>
  <span class="kw">let</span> <span class="fn">expr</span> <span class="op">=</span> <span class="type">Mul</span><span class="punct">(</span><span class="type">Add</span><span class="punct">(</span><span class="type">Num</span><span class="punct">(</span><span class="num">2</span><span class="punct">),</span> <span class="type">Num</span><span class="punct">(</span><span class="num">3</span><span class="punct">)),</span> <span class="type">Neg</span><span class="punct">(</span><span class="type">Num</span><span class="punct">(</span><span class="num">4</span><span class="punct">)))</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"\${show(expr)} = \${eval(expr)}"</span><span class="punct">)</span>
<span class="punct">}</span>`}</code></pre>
            </section>

            <section id="option-result">
                <h2>Option and Result Matching</h2>
                <p>
                    <code>Option&lt;T&gt;</code> and <code>Result&lt;T, E&gt;</code>
                    are sum types built into the language. Match on them to handle
                    missing values and errors.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">script</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">find_user</span><span class="punct">(</span><span class="fn">id</span><span class="op">:</span> <span class="type">Int</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Option&lt;String&gt;</span> <span class="op">=</span>
  <span class="kw">if</span> <span class="fn">id</span> <span class="op">==</span> <span class="num">1</span> <span class="kw">then</span> <span class="type">Some</span><span class="punct">(</span><span class="str">"Alice"</span><span class="punct">)</span>
  <span class="kw">else</span> <span class="kw">if</span> <span class="fn">id</span> <span class="op">==</span> <span class="num">2</span> <span class="kw">then</span> <span class="type">Some</span><span class="punct">(</span><span class="str">"Bob"</span><span class="punct">)</span>
  <span class="kw">else</span> <span class="type">None</span>

<span class="kw">fn</span> <span class="fn">greet_user</span><span class="punct">(</span><span class="fn">id</span><span class="op">:</span> <span class="type">Int</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">String</span> <span class="op">=</span>
  <span class="kw">match</span> <span class="fn">find_user</span><span class="punct">(</span><span class="fn">id</span><span class="punct">)</span>
    <span class="type">Some</span><span class="punct">(</span><span class="fn">name</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="str">"Hello, \${name}!"</span>
    <span class="type">None</span>       <span class="op">-&gt;</span> <span class="str">"User not found"</span>

<span class="kw">fn</span> <span class="fn">parse_and_double</span><span class="punct">(</span><span class="fn">input</span><span class="op">:</span> <span class="type">String</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Result&lt;Int, String&gt;</span> <span class="op">=</span>
  <span class="kw">match</span> <span class="type">Int</span><span class="punct">.</span><span class="fn">parse</span><span class="punct">(</span><span class="fn">input</span><span class="punct">)</span>
    <span class="type">Ok</span><span class="punct">(</span><span class="fn">n</span><span class="punct">)</span>    <span class="op">-&gt;</span> <span class="type">Ok</span><span class="punct">(</span><span class="fn">n</span> <span class="op">*</span> <span class="num">2</span><span class="punct">)</span>
    <span class="type">Err</span><span class="punct">(</span><span class="fn">msg</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Err</span><span class="punct">(</span><span class="str">"Invalid number: \${msg}"</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">main!</span><span class="punct">()</span> <span class="op">-&gt;</span> <span class="effect">{Console}</span> <span class="punct">()</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="fn">greet_user</span><span class="punct">(</span><span class="num">1</span><span class="punct">))</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="fn">greet_user</span><span class="punct">(</span><span class="num">99</span><span class="punct">))</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"\${parse_and_double(<span class="str">"21"</span>)}"</span><span class="punct">)</span>
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
                    <li><a href="#basic-sum-types">Basic Sum Types</a></li>
                    <li><a href="#nested-matching">Nested Patterns</a></li>
                    <li><a href="#option-result">Option and Result</a></li>
                </ul>
            </nav>
        </aside>
    </div>
</main>
