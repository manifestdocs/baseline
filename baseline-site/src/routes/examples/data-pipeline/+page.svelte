<svelte:head>
    <title>Data Pipeline | Baseline Examples</title>
    <meta
        name="description"
        content="List processing in Baseline with pipes, map, filter, fold, and function composition."
    />
    <meta property="og:title" content="Data Pipeline | Baseline Examples" />
    <meta property="og:description" content="List processing in Baseline with pipes, map, filter, fold, and function composition." />
    <meta name="twitter:title" content="Data Pipeline | Baseline Examples" />
    <meta name="twitter:description" content="List processing in Baseline with pipes, map, filter, fold, and function composition." />
</svelte:head>

<main id="main-content" class="pt-[var(--site-top)] pr-8 pb-20 pl-[var(--content-left)]">
    <div class="grid grid-cols-[1fr_14rem] gap-[var(--content-left)] items-start">
        <article class="interior-content guide-content max-w-[48em] min-w-0">
            <h1 class="interior-h1 mb-5">Data Pipeline</h1>
            <p class="interior-intro max-w-[44rem] mb-5">
                The pipe operator <code>|&gt;</code> chains transformations
                left to right. Combined with <code>List.map</code>,
                <code>List.filter</code>, and <code>List.fold</code>, it
                replaces imperative loops with declarative data flow.
            </p>

            <section id="basic-pipes">
                <h2>Basic Pipes</h2>
                <p>
                    The pipe operator passes the left-hand value as the first
                    argument to the right-hand function. Data flows like prose,
                    top to bottom.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">script</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">process_names</span><span class="punct">(</span><span class="fn">names</span><span class="op">:</span> <span class="type">List&lt;String&gt;</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">List&lt;String&gt;</span> <span class="op">=</span>
  <span class="fn">names</span>
  <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">filter</span><span class="punct">(|</span><span class="fn">n</span><span class="punct">|</span> <span class="type">String</span><span class="punct">.</span><span class="fn">length</span><span class="punct">(</span><span class="fn">n</span><span class="punct">)</span> <span class="op">&gt;</span> <span class="num">3</span><span class="punct">)</span>
  <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">map</span><span class="punct">(|</span><span class="fn">n</span><span class="punct">|</span> <span class="type">String</span><span class="punct">.</span><span class="fn">to_upper</span><span class="punct">(</span><span class="fn">n</span><span class="punct">))</span>

<span class="kw">fn</span> <span class="fn">main!</span><span class="punct">()</span> <span class="op">-&gt;</span> <span class="effect">{Console}</span> <span class="punct">()</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">names</span> <span class="op">=</span> <span class="punct">[</span><span class="str">"Al"</span><span class="punct">,</span> <span class="str">"Alice"</span><span class="punct">,</span> <span class="str">"Bob"</span><span class="punct">,</span> <span class="str">"Charlie"</span><span class="punct">,</span> <span class="str">"Jo"</span><span class="punct">]</span>
  <span class="kw">let</span> <span class="fn">result</span> <span class="op">=</span> <span class="fn">process_names</span><span class="punct">(</span><span class="fn">names</span><span class="punct">)</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"Long names: \${result}"</span><span class="punct">)</span>
<span class="punct">}</span>`}</code></pre>
            </section>

            <section id="aggregation">
                <h2>Aggregation with fold</h2>
                <p>
                    <code>List.fold</code> reduces a list to a single value
                    with an accumulator. It replaces reduce, sum, count, and
                    similar operations.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">script</span><span class="punct">)</span>

<span class="kw">type</span> <span class="type">Order</span> <span class="op">=</span> <span class="punct">{</span> <span class="fn">item</span><span class="op">:</span> <span class="type">String</span><span class="punct">,</span> <span class="fn">quantity</span><span class="op">:</span> <span class="type">Int</span><span class="punct">,</span> <span class="fn">price</span><span class="op">:</span> <span class="type">Int</span> <span class="punct">}</span>

<span class="kw">fn</span> <span class="fn">total_revenue</span><span class="punct">(</span><span class="fn">orders</span><span class="op">:</span> <span class="type">List&lt;Order&gt;</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Int</span> <span class="op">=</span>
  <span class="fn">orders</span>
  <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">map</span><span class="punct">(|</span><span class="fn">o</span><span class="punct">|</span> <span class="fn">o</span><span class="punct">.</span><span class="fn">quantity</span> <span class="op">*</span> <span class="fn">o</span><span class="punct">.</span><span class="fn">price</span><span class="punct">)</span>
  <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">fold</span><span class="punct">(</span><span class="num">0</span><span class="punct">,</span> <span class="op">|</span><span class="fn">acc</span><span class="punct">,</span> <span class="fn">x</span><span class="op">|</span> <span class="fn">acc</span> <span class="op">+</span> <span class="fn">x</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">average_price</span><span class="punct">(</span><span class="fn">orders</span><span class="op">:</span> <span class="type">List&lt;Order&gt;</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Int</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">total</span> <span class="op">=</span> <span class="fn">orders</span>
    <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">map</span><span class="punct">(|</span><span class="fn">o</span><span class="punct">|</span> <span class="fn">o</span><span class="punct">.</span><span class="fn">price</span><span class="punct">)</span>
    <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">fold</span><span class="punct">(</span><span class="num">0</span><span class="punct">,</span> <span class="op">|</span><span class="fn">acc</span><span class="punct">,</span> <span class="fn">x</span><span class="op">|</span> <span class="fn">acc</span> <span class="op">+</span> <span class="fn">x</span><span class="punct">)</span>
  <span class="fn">total</span> <span class="op">/</span> <span class="type">List</span><span class="punct">.</span><span class="fn">length</span><span class="punct">(</span><span class="fn">orders</span><span class="punct">)</span>
<span class="punct">}</span>

<span class="kw">fn</span> <span class="fn">expensive_items</span><span class="punct">(</span><span class="fn">orders</span><span class="op">:</span> <span class="type">List&lt;Order&gt;</span><span class="punct">,</span> <span class="fn">threshold</span><span class="op">:</span> <span class="type">Int</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">List&lt;String&gt;</span> <span class="op">=</span>
  <span class="fn">orders</span>
  <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">filter</span><span class="punct">(|</span><span class="fn">o</span><span class="punct">|</span> <span class="fn">o</span><span class="punct">.</span><span class="fn">price</span> <span class="op">&gt;</span> <span class="fn">threshold</span><span class="punct">)</span>
  <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">map</span><span class="punct">(|</span><span class="fn">o</span><span class="punct">|</span> <span class="fn">o</span><span class="punct">.</span><span class="fn">item</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">main!</span><span class="punct">()</span> <span class="op">-&gt;</span> <span class="effect">{Console}</span> <span class="punct">()</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">orders</span> <span class="op">=</span> <span class="punct">[</span>
    <span class="type">Order</span> <span class="punct">{</span> <span class="fn">item</span><span class="op">:</span> <span class="str">"Book"</span><span class="punct">,</span>    <span class="fn">quantity</span><span class="op">:</span> <span class="num">2</span><span class="punct">,</span> <span class="fn">price</span><span class="op">:</span> <span class="num">15</span> <span class="punct">},</span>
    <span class="type">Order</span> <span class="punct">{</span> <span class="fn">item</span><span class="op">:</span> <span class="str">"Laptop"</span><span class="punct">,</span>  <span class="fn">quantity</span><span class="op">:</span> <span class="num">1</span><span class="punct">,</span> <span class="fn">price</span><span class="op">:</span> <span class="num">999</span> <span class="punct">},</span>
    <span class="type">Order</span> <span class="punct">{</span> <span class="fn">item</span><span class="op">:</span> <span class="str">"Pen"</span><span class="punct">,</span>     <span class="fn">quantity</span><span class="op">:</span> <span class="num">10</span><span class="punct">,</span> <span class="fn">price</span><span class="op">:</span> <span class="num">3</span> <span class="punct">},</span>
    <span class="type">Order</span> <span class="punct">{</span> <span class="fn">item</span><span class="op">:</span> <span class="str">"Monitor"</span><span class="punct">,</span> <span class="fn">quantity</span><span class="op">:</span> <span class="num">1</span><span class="punct">,</span> <span class="fn">price</span><span class="op">:</span> <span class="num">450</span> <span class="punct">}</span>
  <span class="punct">]</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"Revenue: \${total_revenue(orders)}"</span><span class="punct">)</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"Avg price: \${average_price(orders)}"</span><span class="punct">)</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"Expensive: \${expensive_items(orders, 100)}"</span><span class="punct">)</span>
<span class="punct">}</span>`}</code></pre>
            </section>

            <section id="multi-step">
                <h2>Multi-step Transformations</h2>
                <p>
                    Chain multiple operations to build complex transformations
                    from simple steps.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">script</span><span class="punct">)</span>

<span class="kw">type</span> <span class="type">Student</span> <span class="op">=</span> <span class="punct">{</span> <span class="fn">name</span><span class="op">:</span> <span class="type">String</span><span class="punct">,</span> <span class="fn">grade</span><span class="op">:</span> <span class="type">Int</span><span class="punct">,</span> <span class="fn">active</span><span class="op">:</span> <span class="type">Boolean</span> <span class="punct">}</span>

<span class="kw">fn</span> <span class="fn">honor_roll</span><span class="punct">(</span><span class="fn">students</span><span class="op">:</span> <span class="type">List&lt;Student&gt;</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">List&lt;String&gt;</span> <span class="op">=</span>
  <span class="fn">students</span>
  <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">filter</span><span class="punct">(|</span><span class="fn">s</span><span class="punct">|</span> <span class="fn">s</span><span class="punct">.</span><span class="fn">active</span><span class="punct">)</span>
  <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">filter</span><span class="punct">(|</span><span class="fn">s</span><span class="punct">|</span> <span class="fn">s</span><span class="punct">.</span><span class="fn">grade</span> <span class="op">&gt;=</span> <span class="num">90</span><span class="punct">)</span>
  <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">map</span><span class="punct">(|</span><span class="fn">s</span><span class="punct">|</span> <span class="str">"\${s.name} (\${s.grade})"</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">class_average</span><span class="punct">(</span><span class="fn">students</span><span class="op">:</span> <span class="type">List&lt;Student&gt;</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Int</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">active</span> <span class="op">=</span> <span class="fn">students</span> <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">filter</span><span class="punct">(|</span><span class="fn">s</span><span class="punct">|</span> <span class="fn">s</span><span class="punct">.</span><span class="fn">active</span><span class="punct">)</span>
  <span class="kw">let</span> <span class="fn">total</span> <span class="op">=</span> <span class="fn">active</span>
    <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">map</span><span class="punct">(|</span><span class="fn">s</span><span class="punct">|</span> <span class="fn">s</span><span class="punct">.</span><span class="fn">grade</span><span class="punct">)</span>
    <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">fold</span><span class="punct">(</span><span class="num">0</span><span class="punct">,</span> <span class="op">|</span><span class="fn">acc</span><span class="punct">,</span> <span class="fn">x</span><span class="op">|</span> <span class="fn">acc</span> <span class="op">+</span> <span class="fn">x</span><span class="punct">)</span>
  <span class="fn">total</span> <span class="op">/</span> <span class="type">List</span><span class="punct">.</span><span class="fn">length</span><span class="punct">(</span><span class="fn">active</span><span class="punct">)</span>
<span class="punct">}</span>

<span class="kw">fn</span> <span class="fn">main!</span><span class="punct">()</span> <span class="op">-&gt;</span> <span class="effect">{Console}</span> <span class="punct">()</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">students</span> <span class="op">=</span> <span class="punct">[</span>
    <span class="type">Student</span> <span class="punct">{</span> <span class="fn">name</span><span class="op">:</span> <span class="str">"Alice"</span><span class="punct">,</span>   <span class="fn">grade</span><span class="op">:</span> <span class="num">95</span><span class="punct">,</span> <span class="fn">active</span><span class="op">:</span> <span class="num">true</span>  <span class="punct">},</span>
    <span class="type">Student</span> <span class="punct">{</span> <span class="fn">name</span><span class="op">:</span> <span class="str">"Bob"</span><span class="punct">,</span>     <span class="fn">grade</span><span class="op">:</span> <span class="num">82</span><span class="punct">,</span> <span class="fn">active</span><span class="op">:</span> <span class="num">true</span>  <span class="punct">},</span>
    <span class="type">Student</span> <span class="punct">{</span> <span class="fn">name</span><span class="op">:</span> <span class="str">"Charlie"</span><span class="punct">,</span> <span class="fn">grade</span><span class="op">:</span> <span class="num">91</span><span class="punct">,</span> <span class="fn">active</span><span class="op">:</span> <span class="num">true</span>  <span class="punct">},</span>
    <span class="type">Student</span> <span class="punct">{</span> <span class="fn">name</span><span class="op">:</span> <span class="str">"Diana"</span><span class="punct">,</span>   <span class="fn">grade</span><span class="op">:</span> <span class="num">97</span><span class="punct">,</span> <span class="fn">active</span><span class="op">:</span> <span class="num">false</span> <span class="punct">},</span>
    <span class="type">Student</span> <span class="punct">{</span> <span class="fn">name</span><span class="op">:</span> <span class="str">"Eve"</span><span class="punct">,</span>     <span class="fn">grade</span><span class="op">:</span> <span class="num">88</span><span class="punct">,</span> <span class="fn">active</span><span class="op">:</span> <span class="num">true</span>  <span class="punct">}</span>
  <span class="punct">]</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"Honor roll: \${honor_roll(students)}"</span><span class="punct">)</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"Class average: \${class_average(students)}"</span><span class="punct">)</span>
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
                    <li><a href="#basic-pipes">Basic Pipes</a></li>
                    <li><a href="#aggregation">Aggregation with fold</a></li>
                    <li><a href="#multi-step">Multi-step Transformations</a></li>
                </ul>
            </nav>
        </aside>
    </div>
</main>
