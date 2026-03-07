<svelte:head>
    <title>Database | Baseline Examples</title>
    <meta
        name="description"
        content="Database queries in Baseline with Sqlite, parameterized queries, and integration with the web framework."
    />
    <meta property="og:title" content="Database | Baseline Examples" />
    <meta property="og:description" content="Database queries in Baseline with Sqlite, parameterized queries, and the web framework." />
    <meta name="twitter:title" content="Database | Baseline Examples" />
    <meta name="twitter:description" content="Database queries in Baseline with Sqlite, parameterized queries, and the web framework." />
</svelte:head>

<main id="main-content" class="pt-[var(--site-top)] pr-8 pb-20 pl-[var(--content-left)]">
    <div class="grid grid-cols-[1fr_14rem] gap-[var(--content-left)] items-start">
        <article class="interior-content guide-content max-w-[48em] min-w-0">
            <h1 class="interior-h1 mb-5">Database</h1>
            <p class="interior-intro max-w-[44rem] mb-5">
                Complete database examples using Sqlite. Baseline provides
                a consistent API across Sqlite, Postgres, and Mysql. All
                database modules require <code>@prelude(server)</code>.
            </p>

            <section id="basic-queries">
                <h2>Basic Queries</h2>
                <p>
                    Connect, create tables, insert data, and query it back.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">server</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">main!</span><span class="punct">()</span> <span class="op">-&gt;</span> <span class="effect">{Sqlite, Console}</span> <span class="type">Unit</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">db</span> <span class="op">=</span> <span class="type">Sqlite</span><span class="punct">.</span><span class="fn">connect!</span><span class="punct">(</span><span class="str">"app.db"</span><span class="punct">)</span>

  <span class="comment">// Create table</span>
  <span class="type">Sqlite</span><span class="punct">.</span><span class="fn">execute!</span><span class="punct">(</span><span class="fn">db</span><span class="punct">,</span>
    <span class="str">"CREATE TABLE IF NOT EXISTS users (
      id INTEGER PRIMARY KEY,
      name TEXT NOT NULL,
      email TEXT NOT NULL
    )"</span><span class="punct">)</span>

  <span class="comment">// Insert with parameterized queries</span>
  <span class="type">Sqlite</span><span class="punct">.</span><span class="fn">execute!</span><span class="punct">(</span><span class="fn">db</span><span class="punct">,</span>
    <span class="str">"INSERT INTO users (name, email) VALUES (?, ?)"</span><span class="punct">,</span>
    <span class="punct">[</span><span class="str">"Alice"</span><span class="punct">,</span> <span class="str">"alice@example.com"</span><span class="punct">])</span>
  <span class="type">Sqlite</span><span class="punct">.</span><span class="fn">execute!</span><span class="punct">(</span><span class="fn">db</span><span class="punct">,</span>
    <span class="str">"INSERT INTO users (name, email) VALUES (?, ?)"</span><span class="punct">,</span>
    <span class="punct">[</span><span class="str">"Bob"</span><span class="punct">,</span> <span class="str">"bob@example.com"</span><span class="punct">])</span>

  <span class="comment">// Query all rows</span>
  <span class="kw">let</span> <span class="fn">users</span> <span class="op">=</span> <span class="type">Sqlite</span><span class="punct">.</span><span class="fn">query!</span><span class="punct">(</span><span class="fn">db</span><span class="punct">,</span> <span class="str">"SELECT * FROM users"</span><span class="punct">)</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"Users: \${users}"</span><span class="punct">)</span>

  <span class="comment">// Query with parameters</span>
  <span class="kw">let</span> <span class="fn">alice</span> <span class="op">=</span> <span class="type">Sqlite</span><span class="punct">.</span><span class="fn">query!</span><span class="punct">(</span><span class="fn">db</span><span class="punct">,</span>
    <span class="str">"SELECT * FROM users WHERE name = ?"</span><span class="punct">,</span> <span class="punct">[</span><span class="str">"Alice"</span><span class="punct">])</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span><span class="punct">(</span><span class="str">"Alice: \${alice}"</span><span class="punct">)</span>
<span class="punct">}</span>`}</code></pre>
            </section>

            <section id="web-integration">
                <h2>Database with Web Framework</h2>
                <p>
                    A typical pattern: connect at startup, pass the connection
                    through request state, and query in handlers.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">server</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">setup_db!</span><span class="punct">(</span><span class="fn">db</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="effect">{Sqlite}</span> <span class="punct">()</span> <span class="op">=</span>
  <span class="type">Sqlite</span><span class="punct">.</span><span class="fn">execute!</span><span class="punct">(</span><span class="fn">db</span><span class="punct">,</span>
    <span class="str">"CREATE TABLE IF NOT EXISTS tasks (
      id INTEGER PRIMARY KEY,
      title TEXT NOT NULL,
      done INTEGER DEFAULT 0
    )"</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">list_tasks</span><span class="punct">(</span><span class="fn">req</span><span class="op">:</span> <span class="type">Request</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Response</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">db</span> <span class="op">=</span> <span class="type">Request</span><span class="punct">.</span><span class="fn">state</span><span class="punct">(</span><span class="fn">req</span><span class="punct">,</span> <span class="str">"db"</span><span class="punct">)</span>
  <span class="kw">let</span> <span class="fn">tasks</span> <span class="op">=</span> <span class="type">Sqlite</span><span class="punct">.</span><span class="fn">query!</span><span class="punct">(</span><span class="fn">db</span><span class="punct">,</span> <span class="str">"SELECT * FROM tasks"</span><span class="punct">)</span>
  <span class="type">Response</span><span class="punct">.</span><span class="fn">json</span><span class="punct">(</span><span class="fn">tasks</span><span class="punct">)</span>
<span class="punct">}</span>

<span class="kw">fn</span> <span class="fn">create_task</span><span class="punct">(</span><span class="fn">req</span><span class="op">:</span> <span class="type">Request</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Response</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">db</span> <span class="op">=</span> <span class="type">Request</span><span class="punct">.</span><span class="fn">state</span><span class="punct">(</span><span class="fn">req</span><span class="punct">,</span> <span class="str">"db"</span><span class="punct">)</span>
  <span class="kw">let</span> <span class="fn">body</span> <span class="op">=</span> <span class="type">Request</span><span class="punct">.</span><span class="fn">body_json</span><span class="punct">(</span><span class="fn">req</span><span class="punct">)</span>
  <span class="type">Sqlite</span><span class="punct">.</span><span class="fn">execute!</span><span class="punct">(</span><span class="fn">db</span><span class="punct">,</span>
    <span class="str">"INSERT INTO tasks (title) VALUES (?)"</span><span class="punct">,</span>
    <span class="punct">[</span><span class="fn">body</span><span class="punct">.</span><span class="fn">title</span><span class="punct">])</span>
  <span class="type">Response</span><span class="punct">.</span><span class="fn">status</span><span class="punct">(</span><span class="num">201</span><span class="punct">,</span> <span class="str">"Created"</span><span class="punct">)</span>
<span class="punct">}</span>

<span class="kw">fn</span> <span class="fn">complete_task</span><span class="punct">(</span><span class="fn">req</span><span class="op">:</span> <span class="type">Request</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Response</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">db</span> <span class="op">=</span> <span class="type">Request</span><span class="punct">.</span><span class="fn">state</span><span class="punct">(</span><span class="fn">req</span><span class="punct">,</span> <span class="str">"db"</span><span class="punct">)</span>
  <span class="kw">let</span> <span class="fn">id</span> <span class="op">=</span> <span class="type">Request</span><span class="punct">.</span><span class="fn">param</span><span class="punct">(</span><span class="fn">req</span><span class="punct">,</span> <span class="str">"id"</span><span class="punct">)</span>
  <span class="type">Sqlite</span><span class="punct">.</span><span class="fn">execute!</span><span class="punct">(</span><span class="fn">db</span><span class="punct">,</span>
    <span class="str">"UPDATE tasks SET done = 1 WHERE id = ?"</span><span class="punct">,</span> <span class="punct">[</span><span class="fn">id</span><span class="punct">])</span>
  <span class="type">Response</span><span class="punct">.</span><span class="fn">ok</span><span class="punct">(</span><span class="str">"Task completed"</span><span class="punct">)</span>
<span class="punct">}</span>

<span class="kw">fn</span> <span class="fn">main!</span><span class="punct">()</span> <span class="op">-&gt;</span> <span class="effect">{Http, Sqlite}</span> <span class="type">Unit</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">db</span> <span class="op">=</span> <span class="type">Sqlite</span><span class="punct">.</span><span class="fn">connect!</span><span class="punct">(</span><span class="str">"tasks.db"</span><span class="punct">)</span>
  <span class="fn">setup_db!</span><span class="punct">(</span><span class="fn">db</span><span class="punct">)</span>
  <span class="kw">let</span> <span class="fn">app</span> <span class="op">=</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">new</span><span class="punct">()</span>
    <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">get</span><span class="punct">(</span><span class="str">"/tasks"</span><span class="punct">,</span> <span class="fn">list_tasks</span><span class="punct">)</span>
    <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">post</span><span class="punct">(</span><span class="str">"/tasks"</span><span class="punct">,</span> <span class="fn">create_task</span><span class="punct">)</span>
    <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">put</span><span class="punct">(</span><span class="str">"/tasks/:id/complete"</span><span class="punct">,</span> <span class="fn">complete_task</span><span class="punct">)</span>
  <span class="type">Log</span><span class="punct">.</span><span class="fn">info!</span><span class="punct">(</span><span class="str">"Task API on :3000"</span><span class="punct">)</span>
  <span class="type">Server</span><span class="punct">.</span><span class="fn">listen!</span><span class="punct">(</span><span class="fn">app</span><span class="punct">,</span> <span class="num">3000</span><span class="punct">)</span>
<span class="punct">}</span>`}</code></pre>
                <p>
                    For a deeper dive into database connections and query
                    patterns, see the <a href="/guides/database">Database Guide</a>.
                </p>
            </section>

            <p class="mt-12 pt-5 border-t border-[var(--fg-faint)]">
                <a href="/examples" class="text-sm text-[var(--fg-dim)] no-underline hover:text-[var(--fg)]">&larr; All Examples</a>
            </p>
        </article>

        <aside class="toc-sidebar sticky top-[var(--site-top)] max-h-[calc(100vh-3.25rem)] overflow-y-auto pb-8" aria-label="Example sections">
            <h2 class="toc-title mb-3">On this page</h2>
            <nav>
                <ul role="list">
                    <li><a href="#basic-queries">Basic Queries</a></li>
                    <li><a href="#web-integration">With Web Framework</a></li>
                </ul>
            </nav>
        </aside>
    </div>
</main>
