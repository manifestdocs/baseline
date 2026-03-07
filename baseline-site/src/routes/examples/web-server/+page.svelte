<svelte:head>
    <title>Web Server | Baseline Examples</title>
    <meta
        name="description"
        content="A complete HTTP server in Baseline with routing, middleware, JSON responses, and error handling."
    />
    <meta property="og:title" content="Web Server | Baseline Examples" />
    <meta property="og:description" content="A complete HTTP server in Baseline with routing, middleware, and JSON responses." />
    <meta name="twitter:title" content="Web Server | Baseline Examples" />
    <meta name="twitter:description" content="A complete HTTP server in Baseline with routing, middleware, and JSON responses." />
</svelte:head>

<main id="main-content" class="pt-[var(--site-top)] pr-8 pb-20 pl-[var(--content-left)]">
    <div class="grid grid-cols-[1fr_14rem] gap-[var(--content-left)] items-start">
        <article class="interior-content guide-content max-w-[48em] min-w-0">
            <h1 class="interior-h1 mb-5">Web Server</h1>
            <p class="interior-intro max-w-[44rem] mb-5">
                A complete HTTP server using Baseline's web framework. This
                example covers routing, request handling, JSON responses,
                middleware, and error handling. All web modules require
                <code>@prelude(server)</code>.
            </p>

            <section id="minimal-server">
                <h2>Minimal Server</h2>
                <p>
                    The smallest possible server: one route, one handler.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">server</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">main!</span><span class="punct">()</span> <span class="op">-&gt;</span> <span class="effect">{Http}</span> <span class="type">Unit</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">app</span> <span class="op">=</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">new</span><span class="punct">()</span>
    <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">get</span><span class="punct">(</span><span class="str">"/"</span><span class="punct">,</span> <span class="op">|</span><span class="fn">req</span><span class="op">|</span> <span class="type">Response</span><span class="punct">.</span><span class="fn">ok</span><span class="punct">(</span><span class="str">"Hello, world!"</span><span class="punct">))</span>
  <span class="type">Server</span><span class="punct">.</span><span class="fn">listen!</span><span class="punct">(</span><span class="fn">app</span><span class="punct">,</span> <span class="num">3000</span><span class="punct">)</span>
<span class="punct">}</span>`}</code></pre>
            </section>

            <section id="rest-api">
                <h2>REST API</h2>
                <p>
                    A JSON API with multiple routes, path parameters, and
                    different HTTP methods.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">server</span><span class="punct">)</span>

<span class="kw">type</span> <span class="type">User</span> <span class="op">=</span> <span class="punct">{</span> <span class="fn">id</span><span class="op">:</span> <span class="type">Int</span><span class="punct">,</span> <span class="fn">name</span><span class="op">:</span> <span class="type">String</span><span class="punct">,</span> <span class="fn">email</span><span class="op">:</span> <span class="type">String</span> <span class="punct">}</span>

<span class="kw">fn</span> <span class="fn">list_users</span><span class="punct">(</span><span class="fn">req</span><span class="op">:</span> <span class="type">Request</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Response</span> <span class="op">=</span>
  <span class="type">Response</span><span class="punct">.</span><span class="fn">json</span><span class="punct">(</span><span class="punct">[</span>
    <span class="type">User</span> <span class="punct">{</span> <span class="fn">id</span><span class="op">:</span> <span class="num">1</span><span class="punct">,</span> <span class="fn">name</span><span class="op">:</span> <span class="str">"Alice"</span><span class="punct">,</span> <span class="fn">email</span><span class="op">:</span> <span class="str">"alice@example.com"</span> <span class="punct">},</span>
    <span class="type">User</span> <span class="punct">{</span> <span class="fn">id</span><span class="op">:</span> <span class="num">2</span><span class="punct">,</span> <span class="fn">name</span><span class="op">:</span> <span class="str">"Bob"</span><span class="punct">,</span> <span class="fn">email</span><span class="op">:</span> <span class="str">"bob@example.com"</span> <span class="punct">}</span>
  <span class="punct">])</span>

<span class="kw">fn</span> <span class="fn">get_user</span><span class="punct">(</span><span class="fn">req</span><span class="op">:</span> <span class="type">Request</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Response</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">id</span> <span class="op">=</span> <span class="type">Request</span><span class="punct">.</span><span class="fn">param</span><span class="punct">(</span><span class="fn">req</span><span class="punct">,</span> <span class="str">"id"</span><span class="punct">)</span>
  <span class="type">Response</span><span class="punct">.</span><span class="fn">json</span><span class="punct">(</span><span class="type">User</span> <span class="punct">{</span> <span class="fn">id</span><span class="op">:</span> <span class="fn">id</span><span class="punct">,</span> <span class="fn">name</span><span class="op">:</span> <span class="str">"Alice"</span><span class="punct">,</span> <span class="fn">email</span><span class="op">:</span> <span class="str">"alice@example.com"</span> <span class="punct">})</span>
<span class="punct">}</span>

<span class="kw">fn</span> <span class="fn">create_user</span><span class="punct">(</span><span class="fn">req</span><span class="op">:</span> <span class="type">Request</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Response</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">body</span> <span class="op">=</span> <span class="type">Request</span><span class="punct">.</span><span class="fn">body_json</span><span class="punct">(</span><span class="fn">req</span><span class="punct">)</span>
  <span class="type">Response</span><span class="punct">.</span><span class="fn">status</span><span class="punct">(</span><span class="num">201</span><span class="punct">,</span> <span class="str">"Created"</span><span class="punct">)</span>
    <span class="op">|&gt;</span> <span class="type">Response</span><span class="punct">.</span><span class="fn">with_header</span><span class="punct">(</span><span class="str">"Location"</span><span class="punct">,</span> <span class="str">"/users/3"</span><span class="punct">)</span>
<span class="punct">}</span>

<span class="kw">fn</span> <span class="fn">delete_user</span><span class="punct">(</span><span class="fn">req</span><span class="op">:</span> <span class="type">Request</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Response</span> <span class="op">=</span>
  <span class="type">Response</span><span class="punct">.</span><span class="fn">no_content</span><span class="punct">()</span>

<span class="kw">fn</span> <span class="fn">main!</span><span class="punct">()</span> <span class="op">-&gt;</span> <span class="effect">{Http}</span> <span class="type">Unit</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">app</span> <span class="op">=</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">new</span><span class="punct">()</span>
    <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">get</span><span class="punct">(</span><span class="str">"/users"</span><span class="punct">,</span> <span class="fn">list_users</span><span class="punct">)</span>
    <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">get</span><span class="punct">(</span><span class="str">"/users/:id"</span><span class="punct">,</span> <span class="fn">get_user</span><span class="punct">)</span>
    <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">post</span><span class="punct">(</span><span class="str">"/users"</span><span class="punct">,</span> <span class="fn">create_user</span><span class="punct">)</span>
    <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">delete</span><span class="punct">(</span><span class="str">"/users/:id"</span><span class="punct">,</span> <span class="fn">delete_user</span><span class="punct">)</span>
  <span class="type">Server</span><span class="punct">.</span><span class="fn">listen!</span><span class="punct">(</span><span class="fn">app</span><span class="punct">,</span> <span class="num">3000</span><span class="punct">)</span>
<span class="punct">}</span>`}</code></pre>
            </section>

            <section id="middleware">
                <h2>With Middleware</h2>
                <p>
                    Add logging, authentication, and CORS headers using
                    middleware. Middleware wraps the handler chain and can
                    modify both requests and responses.
                </p>
                <pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">server</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">logger</span><span class="punct">(</span><span class="fn">req</span><span class="op">:</span> <span class="type">Request</span><span class="punct">,</span> <span class="fn">next</span><span class="punct">)</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">res</span> <span class="op">=</span> <span class="fn">next</span><span class="punct">(</span><span class="fn">req</span><span class="punct">)</span>
  <span class="type">Log</span><span class="punct">.</span><span class="fn">info!</span><span class="punct">(</span><span class="str">"\${Request.method(req)} \${req.path}"</span><span class="punct">)</span>
  <span class="fn">res</span>
<span class="punct">}</span>

<span class="kw">fn</span> <span class="fn">cors</span><span class="punct">(</span><span class="fn">req</span><span class="op">:</span> <span class="type">Request</span><span class="punct">,</span> <span class="fn">next</span><span class="punct">)</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">response</span> <span class="op">=</span> <span class="fn">next</span><span class="punct">(</span><span class="fn">req</span><span class="punct">)</span>
  <span class="fn">response</span>
  <span class="op">|&gt;</span> <span class="type">Response</span><span class="punct">.</span><span class="fn">with_header</span><span class="punct">(</span><span class="str">"Access-Control-Allow-Origin"</span><span class="punct">,</span> <span class="str">"*"</span><span class="punct">)</span>
  <span class="op">|&gt;</span> <span class="type">Response</span><span class="punct">.</span><span class="fn">with_header</span><span class="punct">(</span><span class="str">"Access-Control-Allow-Methods"</span><span class="punct">,</span> <span class="str">"GET, POST, DELETE"</span><span class="punct">)</span>
<span class="punct">}</span>

<span class="kw">fn</span> <span class="fn">auth</span><span class="punct">(</span><span class="fn">req</span><span class="op">:</span> <span class="type">Request</span><span class="punct">,</span> <span class="fn">next</span><span class="punct">)</span> <span class="op">=</span>
  <span class="kw">match</span> <span class="type">Request</span><span class="punct">.</span><span class="fn">header</span><span class="punct">(</span><span class="fn">req</span><span class="punct">,</span> <span class="str">"Authorization"</span><span class="punct">)</span>
    <span class="type">Some</span><span class="punct">(</span><span class="fn">token</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="fn">next</span><span class="punct">(</span><span class="punct">{</span> <span class="op">..</span><span class="fn">req</span><span class="punct">,</span> <span class="fn">user_id</span><span class="op">:</span> <span class="num">1</span> <span class="punct">})</span>
    <span class="type">None</span>        <span class="op">-&gt;</span> <span class="type">Response</span><span class="punct">.</span><span class="fn">status</span><span class="punct">(</span><span class="num">401</span><span class="punct">,</span> <span class="str">"Unauthorized"</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">home</span><span class="punct">(</span><span class="fn">req</span><span class="op">:</span> <span class="type">Request</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Response</span> <span class="op">=</span>
  <span class="type">Response</span><span class="punct">.</span><span class="fn">json</span><span class="punct">(</span><span class="punct">{</span> <span class="fn">message</span><span class="op">:</span> <span class="str">"Welcome!"</span><span class="punct">,</span> <span class="fn">version</span><span class="op">:</span> <span class="str">"1.0"</span> <span class="punct">})</span>

<span class="kw">fn</span> <span class="fn">profile</span><span class="punct">(</span><span class="fn">req</span><span class="op">:</span> <span class="type">Request</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">Response</span> <span class="op">=</span>
  <span class="type">Response</span><span class="punct">.</span><span class="fn">json</span><span class="punct">(</span><span class="punct">{</span> <span class="fn">user_id</span><span class="op">:</span> <span class="fn">req</span><span class="punct">.</span><span class="fn">user_id</span> <span class="punct">})</span>

<span class="kw">fn</span> <span class="fn">main!</span><span class="punct">()</span> <span class="op">-&gt;</span> <span class="effect">{Http}</span> <span class="type">Unit</span> <span class="op">=</span> <span class="punct">{</span>
  <span class="kw">let</span> <span class="fn">app</span> <span class="op">=</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">new</span><span class="punct">()</span>
    <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">use</span><span class="punct">(</span><span class="fn">logger</span><span class="punct">)</span>
    <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">use</span><span class="punct">(</span><span class="fn">cors</span><span class="punct">)</span>
    <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">get</span><span class="punct">(</span><span class="str">"/"</span><span class="punct">,</span> <span class="fn">home</span><span class="punct">)</span>
    <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">group</span><span class="punct">(</span><span class="str">"/api"</span><span class="punct">,</span> <span class="op">|</span><span class="fn">r</span><span class="op">|</span>
      <span class="fn">r</span>
      <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">use</span><span class="punct">(</span><span class="fn">auth</span><span class="punct">)</span>
      <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">get</span><span class="punct">(</span><span class="str">"/profile"</span><span class="punct">,</span> <span class="fn">profile</span><span class="punct">))</span>
  <span class="type">Log</span><span class="punct">.</span><span class="fn">info!</span><span class="punct">(</span><span class="str">"Server starting on :3000"</span><span class="punct">)</span>
  <span class="type">Server</span><span class="punct">.</span><span class="fn">listen!</span><span class="punct">(</span><span class="fn">app</span><span class="punct">,</span> <span class="num">3000</span><span class="punct">)</span>
<span class="punct">}</span>`}</code></pre>
                <p>
                    For a deeper dive into routing, middleware, and response
                    building, see the <a href="/guides/web">Web Framework Guide</a>.
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
                    <li><a href="#minimal-server">Minimal Server</a></li>
                    <li><a href="#rest-api">REST API</a></li>
                    <li><a href="#middleware">With Middleware</a></li>
                </ul>
            </nav>
        </aside>
    </div>
</main>
