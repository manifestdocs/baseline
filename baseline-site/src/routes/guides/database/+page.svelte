<svelte:head>
    <title>Database Guide | Baseline</title>
    <meta
        name="description"
        content="Connect to Sqlite, Postgres, and Mysql databases from Baseline with parameterized queries."
    />
</svelte:head>

<main id="main-content" class="guide-page">
    <div class="guide-layout">
        <article class="guide-content">
            <span class="overline">Guide</span>
            <h1>Database</h1>
            <p class="guide-intro">
                Baseline provides three database modules —
                <strong>Sqlite</strong>, <strong>Postgres</strong>, and
                <strong>Mysql</strong> — each with the same API shape. All
                require <code>@prelude(server)</code>. The <strong>Sql</strong>
                module provides shared query utilities.
            </p>

            <section id="connecting">
                <h2>Connecting</h2>
                <p>
                    Each backend provides a <code>connect!</code> function that opens
                    a connection.
                </p>
                <pre><code
                        >{@html `<span class="annot">@prelude(server)</span>

<span class="comment">-- Sqlite (file-based)</span>
<span class="kw">let</span> <span class="fn">db</span> <span class="op">=</span> <span class="type">Sqlite</span><span class="punct">.</span><span class="fn">connect!</span>(<span class="str">"app.db"</span>)

<span class="comment">-- Postgres</span>
<span class="kw">let</span> <span class="fn">db</span> <span class="op">=</span> <span class="type">Postgres</span><span class="punct">.</span><span class="fn">connect!</span>(<span class="str">"postgres://localhost/mydb"</span>)

<span class="comment">-- Mysql</span>
<span class="kw">let</span> <span class="fn">db</span> <span class="op">=</span> <span class="type">Mysql</span><span class="punct">.</span><span class="fn">connect!</span>(<span class="str">"mysql://localhost/mydb"</span>)`}</code
                    ></pre>
            </section>

            <section id="queries">
                <h2>Running Queries</h2>
                <p>
                    Use <code>query!</code> for SELECT statements that return
                    rows, and <code>execute!</code> for INSERT, UPDATE, DELETE.
                </p>
                <pre><code
                        >{@html `<span class="kw">let</span> <span class="fn">users</span> <span class="op">=</span> <span class="type">Sqlite</span><span class="punct">.</span><span class="fn">query!</span>(<span class="fn">db</span>, <span class="str">"SELECT * FROM users"</span>)

<span class="type">Sqlite</span><span class="punct">.</span><span class="fn">execute!</span>(<span class="fn">db</span>, <span class="str">"INSERT INTO users (name) VALUES ('Alice')"</span>)`}</code
                    ></pre>
            </section>

            <section id="parameters">
                <h2>Parameterized Queries</h2>
                <p>
                    Always use parameterized queries to prevent SQL injection.
                    Pass parameters as a list after the query string.
                </p>
                <pre><code
                        >{@html `<span class="kw">let</span> <span class="fn">user</span> <span class="op">=</span> <span class="type">Sqlite</span><span class="punct">.</span><span class="fn">query!</span>(<span class="fn">db</span>, <span class="str">"SELECT * FROM users WHERE id = ?"</span>, [<span class="fn">id</span>])

<span class="type">Sqlite</span><span class="punct">.</span><span class="fn">execute!</span>(<span class="fn">db</span>, <span class="str">"INSERT INTO users (name, email) VALUES (?, ?)"</span>, [<span class="fn">name</span>, <span class="fn">email</span>])`}</code
                    ></pre>
            </section>

            <section id="with-web">
                <h2>Using with the Web Framework</h2>
                <p>
                    A common pattern is to open the database connection at
                    startup and pass it through request state to handlers.
                </p>
                <pre><code
                        >{@html `<span class="annot">@prelude(server)</span>

<span class="kw">fn</span> <span class="fn">list_users</span>(<span class="fn">req</span>) <span class="op">=</span>
  <span class="kw">let</span> <span class="fn">db</span> <span class="op">=</span> <span class="type">Request</span><span class="punct">.</span><span class="fn">state</span>(<span class="fn">req</span>, <span class="str">"db"</span>)
  <span class="kw">let</span> <span class="fn">users</span> <span class="op">=</span> <span class="type">Sqlite</span><span class="punct">.</span><span class="fn">query!</span>(<span class="fn">db</span>, <span class="str">"SELECT * FROM users"</span>)
  <span class="type">Response</span><span class="punct">.</span><span class="fn">json</span>(<span class="fn">users</span>)

<span class="kw">fn</span> <span class="fn">main!</span>() <span class="op">-&gt;</span> <span class="effect">{Http, Sqlite}</span> <span class="type">Unit</span> <span class="op">=</span>
  <span class="kw">let</span> <span class="fn">db</span> <span class="op">=</span> <span class="type">Sqlite</span><span class="punct">.</span><span class="fn">connect!</span>(<span class="str">"app.db"</span>)
  <span class="kw">let</span> <span class="fn">app</span> <span class="op">=</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">new</span>()
    <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">get</span>(<span class="str">"/users"</span>, <span class="fn">list_users</span>)
  <span class="type">Server</span><span class="punct">.</span><span class="fn">listen!</span>(<span class="fn">app</span>, <span class="num">3000</span>)`}</code
                    ></pre>
            </section>

            <p class="guide-back">
                <a href="/guides">← All Guides</a>
            </p>
        </article>

        <aside class="guide-toc" aria-label="Guide sections">
            <h2 class="guide-toc-title">On this page</h2>
            <nav>
                <ul role="list">
                    <li><a href="#connecting">Connecting</a></li>
                    <li><a href="#queries">Running Queries</a></li>
                    <li><a href="#parameters">Parameterized Queries</a></li>
                    <li>
                        <a href="#with-web">Using with the Web Framework</a>
                    </li>
                </ul>
            </nav>
        </aside>
    </div>
</main>
