<svelte:head>
    <title>Hello World | Baseline Examples</title>
    <meta
        name="description"
        content="Your first Baseline program: Hello World with basic I/O, string interpolation, and the Console effect."
    />
    <meta property="og:title" content="Hello World | Baseline Examples" />
    <meta property="og:description" content="Your first Baseline program: Hello World with basic I/O, string interpolation, and the Console effect." />
    <meta name="twitter:title" content="Hello World | Baseline Examples" />
    <meta name="twitter:description" content="Your first Baseline program: Hello World with basic I/O, string interpolation, and the Console effect." />
</svelte:head>

<main id="main-content" class="pt-[var(--site-top)] pr-8 pb-20 pl-[var(--content-left)]">
    <div class="grid grid-cols-[1fr_14rem] gap-[var(--content-left)] items-start">
        <article class="interior-content guide-content max-w-[48em] min-w-0">
            <h1 class="interior-h1 mb-5">Hello World</h1>
            <p class="interior-intro max-w-[44rem] mb-5">
                Your first Baseline program. This example covers basic output,
                string interpolation, simple functions, and the
                <code>{"{Console}"}</code> effect that tracks I/O.
            </p>

            <section id="minimal">
                <h2>Minimal Hello World</h2>
                <p>
                    The smallest possible program. The <code>@prelude(script)</code>
                    annotation loads the standard library modules needed for scripting.
                    The <code>!</code> suffix on <code>main!</code> marks it as an
                    effectful function, and the <code>{"{Console}"}</code> annotation
                    declares that it performs console I/O.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">script</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">main!</span><span class="punct">()</span> <span class="op">-&gt;</span> <span class="effect">{Console}</span> <span class="punct">()</span> <span class="op">=</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"Hello, World!"</span><span class="punct">)</span>`}</code></pre>
                <p>
                    Run it with <code>blc run hello.bl</code>. The output is
                    simply <code>Hello, World!</code>.
                </p>
            </section>

            <section id="interpolation">
                <h2>String Interpolation</h2>
                <p>
                    Use <code>${"{"}...{"}"}</code> inside strings to embed expressions.
                    There is no string concatenation operator in Baseline; interpolation
                    is the only way to build strings from parts.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">script</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">greet</span><span class="punct">(</span><span class="fn">name</span><span class="op">:</span> <span class="type">String</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">String</span> <span class="op">=</span>
  <span class="str">"Hello, \${name}!"</span>

<span class="kw">fn</span> <span class="fn">main!</span><span class="punct">()</span> <span class="op">-&gt;</span> <span class="effect">{Console}</span> <span class="punct">()</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">name</span> <span class="op">=</span> <span class="str">"Baseline"</span>
  <span class="kw">let</span> <span class="fn">message</span> <span class="op">=</span> <span class="fn">greet</span><span class="punct">(</span><span class="fn">name</span><span class="punct">)</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="fn">message</span><span class="punct">)</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"2 + 3 = \${2 + 3}"</span><span class="punct">)</span>
<span class="punct">}</span>`}</code></pre>
            </section>

            <section id="functions">
                <h2>Functions and Expressions</h2>
                <p>
                    Functions are defined with <code>fn</code>. The body is an
                    expression; the last expression is the return value. There is no
                    <code>return</code> keyword.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">script</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">square</span><span class="punct">(</span><span class="fn">x</span><span class="op">:</span> <span class="type">Int</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Int</span> <span class="op">=</span> <span class="fn">x</span> <span class="op">*</span> <span class="fn">x</span>

<span class="kw">fn</span> <span class="fn">hypotenuse</span><span class="punct">(</span><span class="fn">a</span><span class="op">:</span> <span class="type">Int</span><span class="punct">,</span> <span class="fn">b</span><span class="op">:</span> <span class="type">Int</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Int</span> <span class="op">=</span>
  <span class="fn">square</span><span class="punct">(</span><span class="fn">a</span><span class="punct">)</span> <span class="op">+</span> <span class="fn">square</span><span class="punct">(</span><span class="fn">b</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">classify_age</span><span class="punct">(</span><span class="fn">age</span><span class="op">:</span> <span class="type">Int</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">String</span> <span class="op">=</span>
  <span class="kw">if</span> <span class="fn">age</span> <span class="op">&lt;</span> <span class="num">13</span> <span class="kw">then</span> <span class="str">"child"</span>
  <span class="kw">else</span> <span class="kw">if</span> <span class="fn">age</span> <span class="op">&lt;</span> <span class="num">18</span> <span class="kw">then</span> <span class="str">"teen"</span>
  <span class="kw">else</span> <span class="str">"adult"</span>

<span class="kw">fn</span> <span class="fn">main!</span><span class="punct">()</span> <span class="op">-&gt;</span> <span class="effect">{Console}</span> <span class="punct">()</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"3 squared = \${square(3)}"</span><span class="punct">)</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"hypotenuse(3, 4) = \${hypotenuse(3, 4)}"</span><span class="punct">)</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"Age 15: \${classify_age(15)}"</span><span class="punct">)</span>
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
                    <li><a href="#minimal">Minimal Hello World</a></li>
                    <li><a href="#interpolation">String Interpolation</a></li>
                    <li><a href="#functions">Functions and Expressions</a></li>
                </ul>
            </nav>
        </aside>
    </div>
</main>
