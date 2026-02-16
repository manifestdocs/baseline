<svelte:head>
    <title>AI Agent Tooling Guide | Baseline</title>
    <meta
        name="description"
        content="Use structured diagnostics, built-in docs, verification levels, and constrained generation to build effective AI coding workflows with Baseline."
    />
</svelte:head>

<main id="main-content" class="guide-page">
    <div class="guide-layout">
        <article class="guide-content">
            <span class="overline">Guide</span>
            <h1>AI Agent Tooling</h1>
            <p class="guide-intro">
                The <code>blc</code> compiler provides structured, machine-readable
                output at every stage: checking, testing, documentation lookup, and
                code generation. This guide covers the practical features that AI
                coding agents use when writing Baseline code.
            </p>

            <section id="json-diagnostics">
                <h2>Structured JSON Diagnostics</h2>
                <p>
                    Run <code>blc check --json</code> to get machine-parseable
                    diagnostics. Every diagnostic includes an error code, severity,
                    precise source location, contextual explanation, and fix
                    suggestions with confidence scores.
                </p>
                <pre><code>$ blc check app.bl --json</code></pre>
                <p>
                    For a file with an undefined variable, the output looks like
                    this:
                </p>
                <pre><code
                        >{@html `{
  "status": "failure",
  "verification_level": "refinements",
  "checked": ["types", "refinements"],
  "unchecked": ["specs", "smt"],
  "diagnostics": [
    {
      "code": "TYP_002",
      "severity": "error",
      "location": {
        "file": "app.bl",
        "line": 5,
        "col": 10,
        "end_line": 5,
        "end_col": 13
      },
      "message": "Undefined variable \`nme\`",
      "context": "Variable must be defined before use.",
      "suggestions": [
        {
          "strategy": "replace",
          "description": "Did you mean \`name\`?",
          "confidence": 0.8
        }
      ],
      "source_context": {
        "line_number": 5,
        "source_line": "  let greeting = nme",
        "highlight_col": 9,
        "highlight_len": 3
      }
    }
  ]
}`}</code
                    ></pre>
                <p>
                    The <code>source_context</code> field gives agents the exact
                    source line and highlight position without having to re-read the
                    file. The <code>checked</code> and <code>unchecked</code> arrays
                    tell the agent which analysis passes ran, so it knows what has
                    and has not been verified.
                </p>

                <h3>Diagnostic Fields</h3>
                <p>
                    Every diagnostic in the <code>diagnostics</code> array
                    contains:
                </p>
                <pre><code
                        >code             Error code (TYP_001, REF_001, EFF_001, STY_001, etc.)
severity         "error" | "warning" | "info"
location         File path + line/col + end_line/end_col
message          Primary error description
context          Additional explanation of why this is an error
suggestions      Array of fix strategies (see below)
source_context   Source line + highlight position</code
                    ></pre>

                <h3>Top-level Response Fields</h3>
                <pre><code
                        >status               "ok" | "failure"
verification_level   "types" | "refinements" | "full" | "skip"
checked              List of passes that ran
unchecked            List of passes that did NOT run
diagnostics          Array of diagnostic objects</code
                    ></pre>
            </section>

            <section id="fix-suggestions">
                <h2>Fix Suggestions with Confidence Scores</h2>
                <p>
                    The compiler attaches suggestions to diagnostics. Each
                    suggestion includes a <code>confidence</code> score from 0.0 to
                    1.0 that agents can use to rank repair strategies.
                </p>

                <h3>Undefined Variable (Levenshtein Match)</h3>
                <p>
                    When you reference a variable that does not exist, the compiler
                    finds the closest match by edit distance:
                </p>
                <pre><code
                        >{@html `"suggestions": [
  {
    "strategy": "replace",
    "description": "Did you mean \`name\`?",
    "confidence": 0.8
  }
]`}</code
                    ></pre>

                <h3>Type Coercion Hints</h3>
                <p>
                    When you pass a value of the wrong type, the compiler suggests
                    the conversion function:
                </p>
                <pre><code
                        >{@html `<span class="comment">-- Expected Int, got Float</span>
<span class="str">"Use \`Int.from_float(value)\` to convert Float to Int"</span>  <span class="comment">// confidence: 0.7</span>

<span class="comment">-- Expected String, got Int</span>
<span class="str">"Use string interpolation \`\"\${value}\"\` to convert Int to String"</span>  <span class="comment">// confidence: 0.8</span>`}</code
                    ></pre>

                <h3>Pipe Syntax Suggestion (STY_001)</h3>
                <p>
                    When you nest single-argument function calls, the compiler
                    emits a style warning suggesting pipe syntax:
                </p>
                <pre><code
                        >{@html `<span class="comment">-- This triggers STY_001</span>
<span class="kw">let</span> <span class="fn">result</span> <span class="op">=</span> <span class="type">String</span><span class="punct">.</span><span class="fn">to_upper</span>(<span class="type">String</span><span class="punct">.</span><span class="fn">trim</span>(<span class="fn">input</span>))

<span class="comment">-- Suggested fix:</span>
<span class="kw">let</span> <span class="fn">result</span> <span class="op">=</span> <span class="fn">input</span> <span class="op">|&gt;</span> <span class="type">String</span><span class="punct">.</span><span class="fn">trim</span> <span class="op">|&gt;</span> <span class="type">String</span><span class="punct">.</span><span class="fn">to_upper</span>`}</code
                    ></pre>
                <p>
                    Agents should treat suggestions as ordered by confidence: apply
                    the highest-confidence suggestion first, re-check, and iterate.
                </p>
            </section>

            <section id="built-in-docs">
                <h2>Built-in API Documentation</h2>
                <p>
                    Use <code>blc docs</code> to query the standard library
                    without leaving the terminal. The compiler is the single source
                    of truth for all function signatures, descriptions, and
                    examples.
                </p>

                <h3>Look Up a Specific Function</h3>
                <pre><code>$ blc docs List.map</code></pre>
                <p>Returns the signature, description, effects, prelude level,
                    and a usage example for <code>List.map</code>.</p>

                <h3>Search by Keyword</h3>
                <pre><code>$ blc docs --search "filter"</code></pre>
                <p>
                    Matches against function names, descriptions, signatures,
                    and examples. Case-insensitive substring match.
                </p>

                <h3>JSON Output for Agents</h3>
                <pre><code>$ blc docs List.map --json</code></pre>
                <pre><code
                        >{@html `{
  "modules": [
    {
      "name": "List",
      "description": "Transform collections with map and filter...",
      "category": "language",
      "functions": [
        {
          "name": "map",
          "signature": "fn map(List&lt;T&gt;, (T) -&gt; U) -&gt; List&lt;U&gt;",
          "effects": [],
          "prelude_level": "pure",
          "description": "Apply a function to every element, returning a new list.",
          "example": "[1, 2, 3] |&gt; List.map(|x| x * 2)\\n// =&gt; [2, 4, 6]"
        }
      ]
    }
  ]
}`}</code
                    ></pre>
                <p>
                    Each function entry includes: <code>name</code>,
                    <code>signature</code> (with generic type parameters),
                    <code>effects</code> (which effects the function requires),
                    <code>prelude_level</code> (minimum prelude needed), an
                    optional <code>description</code>, and an optional
                    <code>example</code>.
                </p>
            </section>

            <section id="verification-levels">
                <h2>Verification Levels</h2>
                <p>
                    Use <code>--level</code> to control how much analysis the
                    compiler runs. Faster levels give quicker feedback during
                    iterative development. Deeper levels catch more bugs.
                </p>
                <pre><code
                        >$ blc check app.bl --json --level types         # Type inference only (~ms)
$ blc check app.bl --json --level refinements   # Types + refinements (~100ms) [default]
$ blc check app.bl --json --level full          # Types + refinements + SMT specs (~seconds)
$ blc check app.bl --json --level skip          # Types only, specs unchecked</code
                    ></pre>

                <p>
                    Every JSON response includes <code>verification_level</code>,
                    <code>checked</code>, and <code>unchecked</code> so an agent
                    knows exactly what was verified:
                </p>
                <pre><code
                        >// --level types
"checked": ["types"],
"unchecked": ["refinements", "specs", "smt"]

// --level refinements (default)
"checked": ["types", "refinements"],
"unchecked": ["specs", "smt"]

// --level full
"checked": ["types", "refinements", "specs", "smt"],
"unchecked": []</code
                    ></pre>

                <p>
                    A practical workflow: use <code>--level types</code> for fast
                    iteration while generating code, then run <code>--level
                    refinements</code> for the final check before committing. Use
                    <code>--level full</code> for high-assurance code with
                    refinement types.
                </p>
            </section>

            <section id="llms-txt">
                <h2>llms.txt Reference File</h2>
                <p>
                    The project root contains an <code>llms.txt</code> file: a
                    compact language reference designed for inclusion in system
                    prompts and context windows. It covers syntax, types,
                    patterns, and common mistakes in under 200 lines.
                </p>
                <pre><code
                        >{@html `<span class="comment">-- From llms.txt:</span>

<span class="comment">-- Syntax</span>
<span class="kw">fn</span> <span class="fn">add</span>(<span class="fn">a</span><span class="op">:</span> <span class="type">Int</span>, <span class="fn">b</span><span class="op">:</span> <span class="type">Int</span>) <span class="op">-&gt;</span> <span class="type">Int</span> <span class="op">=</span> <span class="fn">a</span> <span class="op">+</span> <span class="fn">b</span>
<span class="kw">fn</span> <span class="fn">greet!</span>(<span class="fn">name</span><span class="op">:</span> <span class="type">String</span>) <span class="op">-&gt;</span> <span class="type">Unit</span> <span class="op">=</span> <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span>(<span class="str">"Hello, \${name}"</span>)

<span class="comment">-- Types</span>
<span class="kw">type</span> <span class="type">Port</span> <span class="op">=</span> <span class="type">Int</span> <span class="kw">where</span> <span class="fn">self</span> <span class="op">&gt;</span> <span class="num">0</span> <span class="op">&amp;&amp;</span> <span class="fn">self</span> <span class="op">&lt;=</span> <span class="num">65535</span>
<span class="kw">type</span> <span class="type">Status</span> <span class="op">=</span> <span class="op">|</span> <span class="type">Active</span> <span class="op">|</span> <span class="type">Inactive</span> <span class="op">|</span> <span class="type">Error</span>(<span class="type">String</span>)`}</code
                    ></pre>
                <p>
                    The file also includes a "NOT Supported" section that lists
                    constructs agents should never generate:
                </p>
                <pre><code
                        >{@html `## NOT Supported

- No class/extends/implements -- use records + sum types + effects
- No try/catch/throw -- use Result&lt;T, E&gt; with ?
- No null/undefined/nil -- use Option&lt;T&gt;
- No async/await -- effects handle this
- No + for string concat -- use "\${a}\${b}"
- No ! for boolean negation -- use \`not\`
- No value.method() -- use Module.method(value)
- No mutable variables, no return keyword, no semicolons`}</code
                    ></pre>
                <p>
                    Including <code>llms.txt</code> in an agent's context window
                    prevents the most common generation errors: trying to use
                    classes, null, try/catch, or method-on-value call syntax.
                </p>
            </section>

            <section id="inline-tests">
                <h2>Inline Tests</h2>
                <p>
                    Baseline's <code>@test</code> blocks live next to the code they
                    verify. Run them with <code>blc test --json</code> to get
                    structured pass/fail results.
                </p>
                <pre><code
                        >{@html `<span class="kw">fn</span> <span class="fn">clamp</span>(<span class="fn">value</span><span class="op">:</span> <span class="type">Int</span>, <span class="fn">lo</span><span class="op">:</span> <span class="type">Int</span>, <span class="fn">hi</span><span class="op">:</span> <span class="type">Int</span>) <span class="op">-&gt;</span> <span class="type">Int</span> <span class="op">=</span>
  <span class="kw">if</span> <span class="fn">value</span> <span class="op">&lt;</span> <span class="fn">lo</span> <span class="kw">then</span> <span class="fn">lo</span>
  <span class="kw">else</span> <span class="kw">if</span> <span class="fn">value</span> <span class="op">&gt;</span> <span class="fn">hi</span> <span class="kw">then</span> <span class="fn">hi</span>
  <span class="kw">else</span> <span class="fn">value</span>

<span class="annot">@test</span>
<span class="kw">test</span> <span class="str">"below range"</span> <span class="op">=</span> <span class="fn">clamp</span>(<span class="num">-5</span>, <span class="num">0</span>, <span class="num">100</span>) <span class="op">==</span> <span class="num">0</span>
<span class="kw">test</span> <span class="str">"in range"</span> <span class="op">=</span> <span class="fn">clamp</span>(<span class="num">50</span>, <span class="num">0</span>, <span class="num">100</span>) <span class="op">==</span> <span class="num">50</span>
<span class="kw">test</span> <span class="str">"above range"</span> <span class="op">=</span> <span class="fn">clamp</span>(<span class="num">200</span>, <span class="num">0</span>, <span class="num">100</span>) <span class="op">==</span> <span class="num">100</span>`}</code
                    ></pre>
                <pre><code>$ blc test app.bl --json</code></pre>
                <pre><code
                        >{@html `{
  "status": "pass",
  "tests": [
    {
      "name": "below range",
      "status": "pass",
      "location": { "file": "app.bl", "line": 7, "col": 1 }
    },
    {
      "name": "in range",
      "status": "pass",
      "location": { "file": "app.bl", "line": 8, "col": 1 }
    },
    {
      "name": "above range",
      "status": "pass",
      "location": { "file": "app.bl", "line": 9, "col": 1 }
    }
  ],
  "summary": {
    "total": 3,
    "passed": 3,
    "failed": 0
  }
}`}</code
                    ></pre>
                <p>
                    The JSON output includes the test name, pass/fail/skip status,
                    an optional failure message, and the source location. Agents
                    can write tests inline, run <code>blc test --json</code>,
                    and iterate until all tests pass without needing a separate
                    test framework or runner.
                </p>
            </section>

            <section id="cgp">
                <h2>Constrained Generation Protocol</h2>
                <p>
                    The <code>blc cgp</code> command starts an HTTP server that
                    provides token-level type-constrained guidance for LLM
                    inference. The server uses incremental tree-sitter parsing and
                    partial type checking to compute valid next tokens in under
                    10ms per call.
                </p>
                <pre><code>$ blc cgp --port 8765</code></pre>
                <p>The server exposes three endpoints:</p>

                <h3>1. Start a Session</h3>
                <pre><code
                        >{@html `POST /cgp/start
{
  "sessionId": "s1",
  "context": {
    "prelude": "script",
    "bindings": [
      { "name": "users", "type": "List&lt;String&gt;" }
    ],
    "effects": ["Console"],
    "expected_type": "Int",
    "mode": "expression"
  }
}`}</code
                    ></pre>
                <p>
                    Returns the set of valid starting tokens and marks
                    out-of-scope effects as invalid:
                </p>
                <pre><code
                        >{@html `{
  "sessionId": "s1",
  "valid_tokens": ["0", "1", "Console", "List", "Some", "if", "let", "match", "users", ...],
  "invalid_tokens": {
    "Http": "Effect Http not in scope",
    "Fs": "Effect Fs not in scope"
  }
}`}</code
                    ></pre>

                <h3>2. Advance with Tokens</h3>
                <pre><code
                        >{@html `POST /cgp/advance
{
  "sessionId": "s1",
  "tokens": ["List.length(users)"]
}`}</code
                    ></pre>
                <pre><code
                        >{@html `{
  "sessionId": "s1",
  "valid_tokens": ["+", "-", "*", "|&gt;", "==", ...],
  "partial_type": "Int",
  "errors": [],
  "cursor_context": "after_expression",
  "elapsed_us": 847
}`}</code
                    ></pre>
                <p>
                    The response includes: valid next tokens for the current cursor
                    position, the inferred type of the expression so far, any
                    parse or type errors, the syntactic context (expression, type
                    annotation, pattern, field access, etc.), and the computation
                    time in microseconds.
                </p>

                <h3>3. Complete and Validate</h3>
                <pre><code
                        >{@html `POST /cgp/complete
{ "sessionId": "s1" }`}</code
                    ></pre>
                <pre><code
                        >{@html `{
  "sessionId": "s1",
  "status": "valid",
  "diagnostics": []
}`}</code
                    ></pre>
                <p>
                    On completion, the server runs a full parse and type check
                    on the generated source, returning <code>"valid"</code> or
                    <code>"invalid"</code> with diagnostics. The session is then
                    cleaned up.
                </p>

                <h3>Cursor Contexts</h3>
                <p>
                    The <code>cursor_context</code> field tells the agent what
                    kind of token is expected at the current position:
                </p>
                <pre><code
                        >top_level          Between definitions (fn, type, import)
expression         Value position (function body, let RHS, if condition)
type_annotation    After : or -&gt; (type names expected)
pattern            Match arm or let destructuring
field_access       After . (module methods or record fields)
function_args      Inside function call parentheses
effect_set         Inside &#123;Console, ...&#125; effect annotation
after_expression   Operator or continuation expected
match_arm          Inside match expression
function_params    Parameter list of a function</code
                    ></pre>
            </section>

            <section id="one-way">
                <h2>One Way to Do Each Thing</h2>
                <p>
                    Baseline enforces a single syntax for each operation. This
                    reduces the decision space for code generation and means agents
                    do not need to choose between equivalent alternatives.
                </p>
                <pre><code
                        >{@html `<span class="comment">-- Chaining: pipes only (no method chaining, no composition operators)</span>
<span class="fn">items</span> <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">filter</span>(<span class="op">|</span><span class="fn">x</span><span class="op">|</span> <span class="fn">x</span> <span class="op">&gt;</span> <span class="num">0</span>) <span class="op">|&gt;</span> <span class="type">List</span><span class="punct">.</span><span class="fn">map</span>(<span class="op">|</span><span class="fn">x</span><span class="op">|</span> <span class="fn">x</span> <span class="op">*</span> <span class="num">2</span>)

<span class="comment">-- Error handling: Result&lt;T, E&gt; + ? only (no exceptions, no try/catch)</span>
<span class="kw">let</span> <span class="fn">user</span> <span class="op">=</span> <span class="fn">find_user</span>(<span class="fn">id</span>)<span class="op">?</span>

<span class="comment">-- Function calls: Module.method(value) only (no value.method())</span>
<span class="type">String</span><span class="punct">.</span><span class="fn">to_upper</span>(<span class="fn">name</span>)

<span class="comment">-- Optional values: Option&lt;T&gt; only (no null, no undefined)</span>
<span class="kw">match</span> <span class="type">List</span><span class="punct">.</span><span class="fn">head</span>(<span class="fn">items</span>)
  <span class="type">Some</span>(<span class="fn">first</span>) <span class="op">-&gt;</span> <span class="fn">first</span>
  <span class="type">None</span> <span class="op">-&gt;</span> <span class="num">0</span>

<span class="comment">-- Negation: not keyword only (no ! operator for booleans)</span>
<span class="kw">if</span> <span class="kw">not</span> <span class="fn">valid</span> <span class="kw">then</span> <span class="fn">handle_error</span>() <span class="kw">else</span> <span class="fn">proceed</span>()`}</code
                    ></pre>
                <p>
                    The compiler enforces these constraints. If an agent generates
                    <code>value.method()</code>, <code>try/catch</code>, or
                    <code>!flag</code>, the check will fail with a clear error
                    pointing to the correct syntax.
                </p>
            </section>

            <section id="agent-workflow">
                <h2>Putting It Together</h2>
                <p>
                    A typical agent workflow using these features:
                </p>
                <pre><code
                        >1. Load llms.txt into context (language syntax + constraints)
2. Query blc docs --json for available APIs
3. Generate code
4. Run blc check --json --level types for fast feedback
5. If errors: read suggestions, apply highest-confidence fix, repeat
6. Write @test blocks alongside the implementation
7. Run blc test --json to verify correctness
8. Run blc check --json --level refinements for final validation</code
                    ></pre>
                <p>
                    For token-level constrained generation, replace step 3 with the
                    CGP protocol: start a session, advance token by token using
                    the valid token set, and complete when done.
                </p>
            </section>

            <p class="guide-back">
                <a href="/guides">&larr; All Guides</a>
            </p>
        </article>

        <aside class="guide-toc" aria-label="Guide sections">
            <h2 class="guide-toc-title">On this page</h2>
            <nav>
                <ul role="list">
                    <li>
                        <a href="#json-diagnostics">Structured JSON Diagnostics</a>
                    </li>
                    <li>
                        <a href="#fix-suggestions">Fix Suggestions</a>
                    </li>
                    <li>
                        <a href="#built-in-docs">Built-in API Documentation</a>
                    </li>
                    <li>
                        <a href="#verification-levels">Verification Levels</a>
                    </li>
                    <li>
                        <a href="#llms-txt">llms.txt Reference File</a>
                    </li>
                    <li>
                        <a href="#inline-tests">Inline Tests</a>
                    </li>
                    <li>
                        <a href="#cgp">Constrained Generation Protocol</a>
                    </li>
                    <li>
                        <a href="#one-way">One Way to Do Each Thing</a>
                    </li>
                    <li>
                        <a href="#agent-workflow">Putting It Together</a>
                    </li>
                </ul>
            </nav>
        </aside>
    </div>
</main>
