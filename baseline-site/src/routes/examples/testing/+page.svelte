<svelte:head>
    <title>Testing | Baseline Examples</title>
    <meta
        name="description"
        content="Inline tests in Baseline with @test blocks: write tests alongside the code they verify."
    />
    <meta property="og:title" content="Testing | Baseline Examples" />
    <meta property="og:description" content="Inline tests in Baseline with @test blocks." />
    <meta name="twitter:title" content="Testing | Baseline Examples" />
    <meta name="twitter:description" content="Inline tests in Baseline with @test blocks." />
</svelte:head>

<main id="main-content" class="pt-[var(--site-top)] pr-8 pb-20 pl-[var(--content-left)]">
    <div class="grid grid-cols-[1fr_14rem] gap-[var(--content-left)] items-start">
        <article class="interior-content guide-content max-w-[48em] min-w-0">
            <h1 class="interior-h1 mb-5">Testing</h1>
            <p class="interior-intro max-w-[44rem] mb-5">
                Baseline tests live in <code>@test</code> sections inside the
                same file as the code they verify. Tests are plain expressions
                that evaluate to <code>true</code> or <code>false</code>.
                No test framework, no assertions library, no separate test
                files.
            </p>

            <section id="basic-tests">
                <h2>Basic Tests</h2>
                <p>
                    Each test is a named boolean expression. If the expression
                    evaluates to <code>true</code>, the test passes. Tests are
                    grouped under <code>@test</code> and ignored in production
                    builds.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">core</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">add</span><span class="punct">(</span><span class="fn">a</span><span class="op">:</span> <span class="type">Int</span><span class="punct">,</span> <span class="fn">b</span><span class="op">:</span> <span class="type">Int</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Int</span> <span class="op">=</span> <span class="fn">a</span> <span class="op">+</span> <span class="fn">b</span>

<span class="kw">fn</span> <span class="fn">max</span><span class="punct">(</span><span class="fn">a</span><span class="op">:</span> <span class="type">Int</span><span class="punct">,</span> <span class="fn">b</span><span class="op">:</span> <span class="type">Int</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Int</span> <span class="op">=</span>
  <span class="kw">if</span> <span class="fn">a</span> <span class="op">&gt;</span> <span class="fn">b</span> <span class="kw">then</span> <span class="fn">a</span> <span class="kw">else</span> <span class="fn">b</span>

<span class="kw">fn</span> <span class="fn">clamp</span><span class="punct">(</span><span class="fn">value</span><span class="op">:</span> <span class="type">Int</span><span class="punct">,</span> <span class="fn">lo</span><span class="op">:</span> <span class="type">Int</span><span class="punct">,</span> <span class="fn">hi</span><span class="op">:</span> <span class="type">Int</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Int</span> <span class="op">=</span>
  <span class="kw">if</span> <span class="fn">value</span> <span class="op">&lt;</span> <span class="fn">lo</span> <span class="kw">then</span> <span class="fn">lo</span>
  <span class="kw">else</span> <span class="kw">if</span> <span class="fn">value</span> <span class="op">&gt;</span> <span class="fn">hi</span> <span class="kw">then</span> <span class="fn">hi</span>
  <span class="kw">else</span> <span class="fn">value</span>

<span class="attr">@test</span>
<span class="kw">test</span> <span class="str">"add positive"</span>    <span class="op">=</span> <span class="fn">add</span><span class="punct">(</span><span class="num">1</span><span class="punct">,</span> <span class="num">2</span><span class="punct">)</span> <span class="op">==</span> <span class="num">3</span>
<span class="kw">test</span> <span class="str">"add zero"</span>        <span class="op">=</span> <span class="fn">add</span><span class="punct">(</span><span class="num">0</span><span class="punct">,</span> <span class="num">5</span><span class="punct">)</span> <span class="op">==</span> <span class="num">5</span>
<span class="kw">test</span> <span class="str">"add negative"</span>    <span class="op">=</span> <span class="fn">add</span><span class="punct">(</span><span class="num">-3</span><span class="punct">,</span> <span class="num">3</span><span class="punct">)</span> <span class="op">==</span> <span class="num">0</span>
<span class="kw">test</span> <span class="str">"max first"</span>       <span class="op">=</span> <span class="fn">max</span><span class="punct">(</span><span class="num">5</span><span class="punct">,</span> <span class="num">3</span><span class="punct">)</span> <span class="op">==</span> <span class="num">5</span>
<span class="kw">test</span> <span class="str">"max second"</span>      <span class="op">=</span> <span class="fn">max</span><span class="punct">(</span><span class="num">2</span><span class="punct">,</span> <span class="num">7</span><span class="punct">)</span> <span class="op">==</span> <span class="num">7</span>
<span class="kw">test</span> <span class="str">"clamp below"</span>     <span class="op">=</span> <span class="fn">clamp</span><span class="punct">(</span><span class="num">-5</span><span class="punct">,</span> <span class="num">0</span><span class="punct">,</span> <span class="num">100</span><span class="punct">)</span> <span class="op">==</span> <span class="num">0</span>
<span class="kw">test</span> <span class="str">"clamp in range"</span>  <span class="op">=</span> <span class="fn">clamp</span><span class="punct">(</span><span class="num">50</span><span class="punct">,</span> <span class="num">0</span><span class="punct">,</span> <span class="num">100</span><span class="punct">)</span> <span class="op">==</span> <span class="num">50</span>
<span class="kw">test</span> <span class="str">"clamp above"</span>     <span class="op">=</span> <span class="fn">clamp</span><span class="punct">(</span><span class="num">200</span><span class="punct">,</span> <span class="num">0</span><span class="punct">,</span> <span class="num">100</span><span class="punct">)</span> <span class="op">==</span> <span class="num">100</span>`}</code></pre>
            </section>

            <section id="testing-types">
                <h2>Testing Sum Types and Results</h2>
                <p>
                    Tests can match on <code>Result</code> and
                    <code>Option</code> values. Compare entire constructed
                    values with <code>==</code>.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">core</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">safe_divide</span><span class="punct">(</span><span class="fn">a</span><span class="op">:</span> <span class="type">Int</span><span class="punct">,</span> <span class="fn">b</span><span class="op">:</span> <span class="type">Int</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Result&lt;Int, String&gt;</span> <span class="op">=</span>
  <span class="kw">if</span> <span class="fn">b</span> <span class="op">==</span> <span class="num">0</span> <span class="kw">then</span> <span class="type">Err</span><span class="punct">(</span><span class="str">"Division by zero"</span><span class="punct">)</span>
  <span class="kw">else</span> <span class="type">Ok</span><span class="punct">(</span><span class="fn">a</span> <span class="op">/</span> <span class="fn">b</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">first_positive</span><span class="punct">(</span><span class="fn">xs</span><span class="op">:</span> <span class="type">List&lt;Int&gt;</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Option&lt;Int&gt;</span> <span class="op">=</span>
  <span class="fn">xs</span>
  <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">filter</span><span class="punct">(|</span><span class="fn">x</span><span class="punct">|</span> <span class="fn">x</span> <span class="op">&gt;</span> <span class="num">0</span><span class="punct">)</span>
  <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">head</span>

<span class="kw">type</span> <span class="type">Direction</span> <span class="op">=</span> <span class="op">|</span> <span class="type">North</span> <span class="op">|</span> <span class="type">South</span> <span class="op">|</span> <span class="type">East</span> <span class="op">|</span> <span class="type">West</span>

<span class="kw">fn</span> <span class="fn">opposite</span><span class="punct">(</span><span class="fn">d</span><span class="op">:</span> <span class="type">Direction</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Direction</span> <span class="op">=</span>
  <span class="kw">match</span> <span class="fn">d</span>
    <span class="type">North</span> <span class="op">-&gt;</span> <span class="type">South</span>
    <span class="type">South</span> <span class="op">-&gt;</span> <span class="type">North</span>
    <span class="type">East</span>  <span class="op">-&gt;</span> <span class="type">West</span>
    <span class="type">West</span>  <span class="op">-&gt;</span> <span class="type">East</span>

<span class="attr">@test</span>
<span class="kw">test</span> <span class="str">"divides ok"</span>        <span class="op">=</span> <span class="fn">safe_divide</span><span class="punct">(</span><span class="num">10</span><span class="punct">,</span> <span class="num">2</span><span class="punct">)</span> <span class="op">==</span> <span class="type">Ok</span><span class="punct">(</span><span class="num">5</span><span class="punct">)</span>
<span class="kw">test</span> <span class="str">"div by zero"</span>       <span class="op">=</span> <span class="fn">safe_divide</span><span class="punct">(</span><span class="num">1</span><span class="punct">,</span> <span class="num">0</span><span class="punct">)</span> <span class="op">==</span> <span class="type">Err</span><span class="punct">(</span><span class="str">"Division by zero"</span><span class="punct">)</span>
<span class="kw">test</span> <span class="str">"finds positive"</span>    <span class="op">=</span> <span class="fn">first_positive</span><span class="punct">(</span><span class="punct">[</span><span class="num">-1</span><span class="punct">,</span> <span class="num">-2</span><span class="punct">,</span> <span class="num">3</span><span class="punct">,</span> <span class="num">4</span><span class="punct">])</span> <span class="op">==</span> <span class="type">Some</span><span class="punct">(</span><span class="num">3</span><span class="punct">)</span>
<span class="kw">test</span> <span class="str">"no positive"</span>      <span class="op">=</span> <span class="fn">first_positive</span><span class="punct">(</span><span class="punct">[</span><span class="num">-1</span><span class="punct">,</span> <span class="num">-2</span><span class="punct">])</span> <span class="op">==</span> <span class="type">None</span>
<span class="kw">test</span> <span class="str">"opposite north"</span>    <span class="op">=</span> <span class="fn">opposite</span><span class="punct">(</span><span class="type">North</span><span class="punct">)</span> <span class="op">==</span> <span class="type">South</span>
<span class="kw">test</span> <span class="str">"opposite east"</span>     <span class="op">=</span> <span class="fn">opposite</span><span class="punct">(</span><span class="type">East</span><span class="punct">)</span> <span class="op">==</span> <span class="type">West</span>
<span class="kw">test</span> <span class="str">"roundtrip"</span>         <span class="op">=</span> <span class="fn">opposite</span><span class="punct">(</span><span class="fn">opposite</span><span class="punct">(</span><span class="type">North</span><span class="punct">))</span> <span class="op">==</span> <span class="type">North</span>`}</code></pre>
            </section>

            <section id="running-tests">
                <h2>Running Tests</h2>
                <p>
                    Use <code>blc test</code> to run all tests in a file.
                    Add <code>--json</code> for structured output that AI
                    agents and CI systems can parse.
                </p>
                <pre><code>{@html `<span class="comment">// Run tests</span>
$ blc test math.bl

<span class="comment">// JSON output for CI/agents</span>
$ blc test math.bl --json`}</code></pre>
                <p>The JSON output includes each test's name, status, and
                    source location:</p>
                <pre><code>{@html `{
  "status": "pass",
  "tests": [
    { "name": "add positive",   "status": "pass", "location": { "file": "math.bl", "line": 12 } },
    { "name": "add zero",       "status": "pass", "location": { "file": "math.bl", "line": 13 } },
    { "name": "clamp below",    "status": "pass", "location": { "file": "math.bl", "line": 16 } }
  ],
  "summary": { "total": 8, "passed": 8, "failed": 0 }
}`}</code></pre>
            </section>

            <p class="mt-12 pt-5 border-t border-[var(--fg-faint)]">
                <a href="/examples" class="text-sm text-[var(--fg-dim)] no-underline hover:text-[var(--fg)]">&larr; All Examples</a>
            </p>
        </article>

        <aside class="toc-sidebar sticky top-[var(--site-top)] max-h-[calc(100vh-3.25rem)] overflow-y-auto pb-8" aria-label="Example sections">
            <h2 class="toc-title mb-3">On this page</h2>
            <nav>
                <ul role="list">
                    <li><a href="#basic-tests">Basic Tests</a></li>
                    <li><a href="#testing-types">Testing Sum Types</a></li>
                    <li><a href="#running-tests">Running Tests</a></li>
                </ul>
            </nav>
        </aside>
    </div>
</main>
