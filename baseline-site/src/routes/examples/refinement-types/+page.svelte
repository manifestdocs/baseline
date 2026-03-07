<svelte:head>
    <title>Refinement Types | Baseline Examples</title>
    <meta
        name="description"
        content="Compile-time validation with refinement types in Baseline: integer constraints, the where clause, and proving invariants."
    />
    <meta property="og:title" content="Refinement Types | Baseline Examples" />
    <meta property="og:description" content="Compile-time validation with refinement types in Baseline." />
    <meta name="twitter:title" content="Refinement Types | Baseline Examples" />
    <meta name="twitter:description" content="Compile-time validation with refinement types in Baseline." />
</svelte:head>

<main id="main-content" class="pt-[var(--site-top)] pr-8 pb-20 pl-[var(--content-left)]">
    <div class="grid grid-cols-[1fr_14rem] gap-[var(--content-left)] items-start">
        <article class="interior-content guide-content max-w-[48em] min-w-0">
            <h1 class="interior-h1 mb-5">Refinement Types</h1>
            <p class="interior-intro max-w-[44rem] mb-5">
                Refinement types add constraints to existing types that the
                compiler proves at compile time. Instead of writing runtime
                validation checks, you encode the constraints in the type
                itself. If the program compiles, the constraints hold.
            </p>

            <section id="basic-refinements">
                <h2>Basic Refinements</h2>
                <p>
                    Use <code>where</code> to add integer constraints to a type.
                    The compiler proves that values satisfy the constraint at
                    every use site.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">script</span><span class="punct">)</span>

<span class="comment">// Port numbers must be between 1 and 65535</span>
<span class="kw">type</span> <span class="type">Port</span> <span class="op">=</span> <span class="type">Int</span> <span class="kw">where</span> <span class="fn">self</span> <span class="op">&gt;=</span> <span class="num">1</span> <span class="op">&amp;&amp;</span> <span class="fn">self</span> <span class="op">&lt;=</span> <span class="num">65535</span>

<span class="comment">// Positive integers only</span>
<span class="kw">type</span> <span class="type">PosInt</span> <span class="op">=</span> <span class="type">Int</span> <span class="kw">where</span> <span class="fn">self</span> <span class="op">&gt;</span> <span class="num">0</span>

<span class="comment">// Percentage: 0 to 100 inclusive</span>
<span class="kw">type</span> <span class="type">Percentage</span> <span class="op">=</span> <span class="type">Int</span> <span class="kw">where</span> <span class="fn">self</span> <span class="op">&gt;=</span> <span class="num">0</span> <span class="op">&amp;&amp;</span> <span class="fn">self</span> <span class="op">&lt;=</span> <span class="num">100</span>

<span class="kw">fn</span> <span class="fn">start_server!</span><span class="punct">(</span><span class="fn">port</span><span class="op">:</span> <span class="type">Port</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="effect">{Console}</span> <span class="punct">()</span> <span class="op">=</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"Listening on port \${port}"</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">show_progress!</span><span class="punct">(</span><span class="fn">pct</span><span class="op">:</span> <span class="type">Percentage</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="effect">{Console}</span> <span class="punct">()</span> <span class="op">=</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"Progress: \${pct}%"</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">main!</span><span class="punct">()</span> <span class="op">-&gt;</span> <span class="effect">{Console}</span> <span class="punct">()</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="fn">start_server!</span><span class="punct">(</span><span class="num">8080</span><span class="punct">)</span>   <span class="comment">// Compiles: 8080 is in 1..65535</span>
  <span class="fn">show_progress!</span><span class="punct">(</span><span class="num">75</span><span class="punct">)</span>    <span class="comment">// Compiles: 75 is in 0..100</span>
  <span class="comment">// start_server!(0)     -- Would NOT compile: 0 &lt; 1</span>
  <span class="comment">// show_progress!(101)  -- Would NOT compile: 101 &gt; 100</span>
<span class="punct">}</span>`}</code></pre>
            </section>

            <section id="domain-modeling">
                <h2>Domain Modeling</h2>
                <p>
                    Combine refinement types with records and sum types to
                    create precise domain models where invalid states are
                    unrepresentable.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">script</span><span class="punct">)</span>

<span class="kw">type</span> <span class="type">Age</span> <span class="op">=</span> <span class="type">Int</span> <span class="kw">where</span> <span class="fn">self</span> <span class="op">&gt;=</span> <span class="num">0</span> <span class="op">&amp;&amp;</span> <span class="fn">self</span> <span class="op">&lt;=</span> <span class="num">150</span>
<span class="kw">type</span> <span class="type">Score</span> <span class="op">=</span> <span class="type">Int</span> <span class="kw">where</span> <span class="fn">self</span> <span class="op">&gt;=</span> <span class="num">0</span> <span class="op">&amp;&amp;</span> <span class="fn">self</span> <span class="op">&lt;=</span> <span class="num">100</span>
<span class="kw">type</span> <span class="type">Year</span> <span class="op">=</span> <span class="type">Int</span> <span class="kw">where</span> <span class="fn">self</span> <span class="op">&gt;=</span> <span class="num">1900</span> <span class="op">&amp;&amp;</span> <span class="fn">self</span> <span class="op">&lt;=</span> <span class="num">2100</span>

<span class="kw">type</span> <span class="type">Student</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="fn">name</span><span class="op">:</span> <span class="type">String</span><span class="punct">,</span>
  <span class="fn">age</span><span class="op">:</span> <span class="type">Age</span><span class="punct">,</span>
  <span class="fn">gpa</span><span class="op">:</span> <span class="type">Score</span><span class="punct">,</span>
  <span class="fn">enrollment_year</span><span class="op">:</span> <span class="type">Year</span>
<span class="punct">}</span>

<span class="kw">fn</span> <span class="fn">is_honor_student</span><span class="punct">(</span><span class="fn">s</span><span class="op">:</span> <span class="type">Student</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Boolean</span> <span class="op">=</span>
  <span class="fn">s</span><span class="punct">.</span><span class="fn">gpa</span> <span class="op">&gt;=</span> <span class="num">90</span>

<span class="kw">fn</span> <span class="fn">main!</span><span class="punct">()</span> <span class="op">-&gt;</span> <span class="effect">{Console}</span> <span class="punct">()</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">alice</span> <span class="op">=</span> <span class="type">Student</span> <span class="punct">{</span>
    <span class="fn">name</span><span class="op">:</span> <span class="str">"Alice"</span><span class="punct">,</span>
    <span class="fn">age</span><span class="op">:</span> <span class="num">20</span><span class="punct">,</span>
    <span class="fn">gpa</span><span class="op">:</span> <span class="num">95</span><span class="punct">,</span>
    <span class="fn">enrollment_year</span><span class="op">:</span> <span class="num">2024</span>
  <span class="punct">}</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"\${alice.name}: honor = \${is_honor_student(alice)}"</span><span class="punct">)</span>
<span class="punct">}</span>`}</code></pre>
            </section>

            <section id="compile-errors">
                <h2>What the Compiler Catches</h2>
                <p>
                    When a value provably violates a refinement, the compiler
                    rejects the program with a clear error. You never need to
                    write runtime validation for these constraints.
                </p>
                <pre><code>{@html `<span class="comment">// These would produce compile-time errors:</span>

<span class="comment">// REF_001: Refinement violation: self &gt;= 1 not satisfied</span>
<span class="comment">// let bad_port: Port = 0</span>

<span class="comment">// REF_001: Refinement violation: self &lt;= 65535 not satisfied</span>
<span class="comment">// let huge_port: Port = 70000</span>

<span class="comment">// REF_001: Refinement violation: self &gt;= 0 not satisfied</span>
<span class="comment">// let bad_age: Age = -1</span>

<span class="comment">// The compiler output includes the constraint that failed,</span>
<span class="comment">// the value that violated it, and the source location.</span>`}</code></pre>
            </section>

            <p class="mt-12 pt-5 border-t border-[var(--fg-faint)]">
                <a href="/examples" class="text-sm text-[var(--fg-dim)] no-underline hover:text-[var(--fg)]">&larr; All Examples</a>
            </p>
        </article>

        <aside class="toc-sidebar sticky top-[var(--site-top)] max-h-[calc(100vh-3.25rem)] overflow-y-auto pb-8" aria-label="Example sections">
            <h2 class="toc-title mb-3">On this page</h2>
            <nav>
                <ul role="list">
                    <li><a href="#basic-refinements">Basic Refinements</a></li>
                    <li><a href="#domain-modeling">Domain Modeling</a></li>
                    <li><a href="#compile-errors">What the Compiler Catches</a></li>
                </ul>
            </nav>
        </aside>
    </div>
</main>
