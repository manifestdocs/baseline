<svelte:head>
    <title>Effects | Baseline Examples</title>
    <meta
        name="description"
        content="Tracked side effects in Baseline: effect annotations, the effect system, and how the compiler enforces what your code can do."
    />
    <meta property="og:title" content="Effects | Baseline Examples" />
    <meta property="og:description" content="Tracked side effects in Baseline: effect annotations and the effect system." />
    <meta name="twitter:title" content="Effects | Baseline Examples" />
    <meta name="twitter:description" content="Tracked side effects in Baseline: effect annotations and the effect system." />
</svelte:head>

<main id="main-content" class="pt-[var(--site-top)] pr-8 pb-20 pl-[var(--content-left)]">
    <div class="grid grid-cols-[1fr_14rem] gap-[var(--content-left)] items-start">
        <article class="interior-content guide-content max-w-[48em] min-w-0">
            <h1 class="interior-h1 mb-5">Effects</h1>
            <p class="interior-intro max-w-[44rem] mb-5">
                In Baseline, every side effect is declared in the function
                signature. <code>{"{Console}"}</code> means the function
                prints. <code>{"{Http}"}</code> means it makes network
                requests. The compiler tracks these transitively: if function
                A calls function B, A must declare all of B's effects too.
            </p>

            <section id="basic-effects">
                <h2>Basic Effect Annotations</h2>
                <p>
                    Effectful functions use the <code>!</code> suffix on their
                    name. The effect set appears between the return arrow and
                    the return type.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">script</span><span class="punct">)</span>

<span class="comment">// Pure function: no effects, no ! suffix</span>
<span class="kw">fn</span> <span class="fn">add</span><span class="punct">(</span><span class="fn">a</span><span class="op">:</span> <span class="type">Int</span><span class="punct">,</span> <span class="fn">b</span><span class="op">:</span> <span class="type">Int</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Int</span> <span class="op">=</span> <span class="fn">a</span> <span class="op">+</span> <span class="fn">b</span>

<span class="comment">// Effectful: prints to the console</span>
<span class="kw">fn</span> <span class="fn">greet!</span><span class="punct">(</span><span class="fn">name</span><span class="op">:</span> <span class="type">String</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="effect">{Console}</span> <span class="punct">()</span> <span class="op">=</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"Hello, \${name}!"</span><span class="punct">)</span>

<span class="comment">// Ambient effect: Log does not need a declaration</span>
<span class="kw">fn</span> <span class="fn">log_request!</span><span class="punct">(</span><span class="fn">path</span><span class="op">:</span> <span class="type">String</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="punct">()</span> <span class="op">=</span>
  <span class="type">Log</span><span class="punct">.</span><span class="fn">info!</span><span class="punct">(</span><span class="str">"Received: \${path}"</span><span class="punct">)</span>

<span class="comment">// Multiple effects: both console and HTTP</span>
<span class="kw">fn</span> <span class="fn">fetch_and_log!</span><span class="punct">(</span><span class="fn">url</span><span class="op">:</span> <span class="type">String</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="effect">{Http, Console}</span> <span class="type">Result&lt;String, String&gt;</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"Fetching \${url}"</span><span class="punct">)</span>
  <span class="kw">let</span> <span class="fn">response</span> <span class="op">=</span> <span class="type">Http</span><span class="punct">.</span><span class="fn">get!</span><span class="punct">(</span><span class="fn">url</span><span class="punct">)</span><span class="op">?</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"Status: \${response.status}"</span><span class="punct">)</span>
  <span class="type">Ok</span><span class="punct">(</span><span class="fn">response</span><span class="punct">.</span><span class="fn">body</span><span class="punct">)</span>
<span class="punct">}</span>`}</code></pre>
            </section>

            <section id="transitive">
                <h2>Transitive Effect Tracking</h2>
                <p>
                    If your function calls an effectful function, the compiler
                    requires you to declare those effects too. Effects propagate
                    up the call chain automatically.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">script</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">notify!</span><span class="punct">(</span><span class="fn">msg</span><span class="op">:</span> <span class="type">String</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="effect">{Console}</span> <span class="punct">()</span> <span class="op">=</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"[LOG] \${msg}"</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">write_file!</span><span class="punct">(</span><span class="fn">path</span><span class="op">:</span> <span class="type">String</span><span class="punct">,</span> <span class="fn">content</span><span class="op">:</span> <span class="type">String</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="effect">{Fs, Console}</span> <span class="punct">()</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="fn">notify!</span><span class="punct">(</span><span class="str">"Writing to \${path}"</span><span class="punct">)</span>
  <span class="type">Fs</span><span class="punct">.</span><span class="fn">write!</span><span class="punct">(</span><span class="fn">path</span><span class="punct">,</span> <span class="fn">content</span><span class="punct">)</span>
<span class="punct">}</span>

<span class="comment">// This function calls both notify! and write_file!</span>
<span class="comment">// so it must declare their non-ambient effects: {Fs, Console}</span>
<span class="kw">fn</span> <span class="fn">save_report!</span><span class="punct">(</span><span class="fn">data</span><span class="op">:</span> <span class="type">String</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="effect">{Fs, Console}</span> <span class="punct">()</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="fn">notify!</span><span class="punct">(</span><span class="str">"Generating report"</span><span class="punct">)</span>
  <span class="kw">let</span> <span class="fn">report</span> <span class="op">=</span> <span class="str">"Report: \${data}"</span>
  <span class="fn">write_file!</span><span class="punct">(</span><span class="str">"report.txt"</span><span class="punct">,</span> <span class="fn">report</span><span class="punct">)</span>
  <span class="fn">notify!</span><span class="punct">(</span><span class="str">"Report saved"</span><span class="punct">)</span>
<span class="punct">}</span>

<span class="kw">fn</span> <span class="fn">main!</span><span class="punct">()</span> <span class="op">-&gt;</span> <span class="effect">{Fs, Console}</span> <span class="punct">()</span> <span class="op">=</span>
  <span class="fn">save_report!</span><span class="punct">(</span><span class="str">"Q4 Revenue: 1.2M"</span><span class="punct">)</span>`}</code></pre>
            </section>

            <section id="mixed">
                <h2>Mixing Pure and Effectful Code</h2>
                <p>
                    Pure functions can be called from anywhere. Effectful
                    functions can only be called from other effectful functions
                    that declare the required effects. This creates a clear
                    boundary between computation and I/O.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">script</span><span class="punct">)</span>

<span class="comment">// Pure: transforms data without side effects</span>
<span class="kw">fn</span> <span class="fn">format_table</span><span class="punct">(</span><span class="fn">rows</span><span class="op">:</span> <span class="type">List&lt;String&gt;</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">String</span> <span class="op">=</span>
  <span class="fn">rows</span>
  <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">map</span><span class="punct">(|</span><span class="fn">r</span><span class="punct">|</span> <span class="str">"| \${r} |"</span><span class="punct">)</span>
  <span class="op">|&gt;</span> <span class="type">String</span><span class="punct">.</span><span class="fn">join</span><span class="punct">(</span><span class="str">"\\n"</span><span class="punct">)</span>

<span class="comment">// Pure: computes a summary from data</span>
<span class="kw">fn</span> <span class="fn">summarize</span><span class="punct">(</span><span class="fn">values</span><span class="op">:</span> <span class="type">List&lt;Int&gt;</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">String</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">total</span> <span class="op">=</span> <span class="fn">values</span> <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">fold</span><span class="punct">(</span><span class="num">0</span><span class="punct">,</span> <span class="op">|</span><span class="fn">a</span><span class="punct">,</span> <span class="fn">b</span><span class="op">|</span> <span class="fn">a</span> <span class="op">+</span> <span class="fn">b</span><span class="punct">)</span>
  <span class="kw">let</span> <span class="fn">count</span> <span class="op">=</span> <span class="type">List</span><span class="punct">.</span><span class="fn">length</span><span class="punct">(</span><span class="fn">values</span><span class="punct">)</span>
  <span class="str">"Total: \${total}, Count: \${count}, Avg: \${total / count}"</span>
<span class="punct">}</span>

<span class="comment">// Effectful: uses pure functions, then prints result</span>
<span class="kw">fn</span> <span class="fn">main!</span><span class="punct">()</span> <span class="op">-&gt;</span> <span class="effect">{Console}</span> <span class="punct">()</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">data</span> <span class="op">=</span> <span class="punct">[</span><span class="num">10</span><span class="punct">,</span> <span class="num">20</span><span class="punct">,</span> <span class="num">30</span><span class="punct">,</span> <span class="num">40</span><span class="punct">,</span> <span class="num">50</span><span class="punct">]</span>
  <span class="kw">let</span> <span class="fn">report</span> <span class="op">=</span> <span class="fn">summarize</span><span class="punct">(</span><span class="fn">data</span><span class="punct">)</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="fn">report</span><span class="punct">)</span>
  <span class="kw">let</span> <span class="fn">table</span> <span class="op">=</span> <span class="fn">format_table</span><span class="punct">(</span><span class="punct">[</span><span class="str">"Alice: 95"</span><span class="punct">,</span> <span class="str">"Bob: 82"</span><span class="punct">,</span> <span class="str">"Charlie: 91"</span><span class="punct">])</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="fn">table</span><span class="punct">)</span>
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
                    <li><a href="#basic-effects">Basic Effect Annotations</a></li>
                    <li><a href="#transitive">Transitive Tracking</a></li>
                    <li><a href="#mixed">Mixing Pure and Effectful</a></li>
                </ul>
            </nav>
        </aside>
    </div>
</main>
