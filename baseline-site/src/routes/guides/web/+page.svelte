<svelte:head>
    <title>Web Framework Guide | Baseline</title>
    <meta
        name="description"
        content="Build HTTP servers in Baseline with routing, middleware, JSON responses, and error handling."
    />
</svelte:head>

<main id="main-content" class="guide-page">
    <div class="guide-layout">
        <article class="guide-content">
            <span class="overline">Guide</span>
            <h1>Web Framework</h1>
            <p class="guide-intro">
                Baseline's web framework is a small set of modules that work
                together: <strong>Router</strong> defines URL patterns,
                <strong>Server</strong> runs the HTTP listener,
                <strong>Request</strong> and <strong>Response</strong> handle
                the data flow, and <strong>Middleware</strong> adds
                cross-cutting concerns. All require
                <code>@prelude(server)</code>.
            </p>

            <section id="hello-world">
                <h2>Hello World</h2>
                <p>
                    The smallest possible server. Create a router, register a
                    single route, and start listening.
                </p>
                <pre><code
                        >{@html `<span class="annot">@prelude(server)</span>

<span class="kw">fn</span> <span class="fn">main!</span>() <span class="op">-&gt;</span> <span class="effect">{Http}</span> <span class="type">Unit</span> <span class="op">=</span>
  <span class="kw">let</span> <span class="fn">app</span> <span class="op">=</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">new</span>()
    <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">get</span>(<span class="str">"/"</span>, <span class="op">|</span><span class="fn">req</span><span class="op">|</span> <span class="type">Response</span><span class="punct">.</span><span class="fn">ok</span>(<span class="str">"Hello, world!"</span>))
  <span class="type">Server</span><span class="punct">.</span><span class="fn">listen!</span>(<span class="fn">app</span>, <span class="num">3000</span>)`}</code
                    ></pre>
            </section>

            <section id="routing">
                <h2>Routing</h2>
                <p>
                    Register handlers for HTTP methods with
                    <code>Router.get</code>, <code>.post</code>,
                    <code>.put</code>, <code>.delete</code>, and
                    <code>.patch</code>. Use <code>Router.any</code> to match all
                    methods.
                </p>
                <pre><code
                        >{@html `<span class="kw">let</span> <span class="fn">app</span> <span class="op">=</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">new</span>()
  <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">get</span>(<span class="str">"/users"</span>, <span class="fn">list_users</span>)
  <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">post</span>(<span class="str">"/users"</span>, <span class="fn">create_user</span>)
  <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">get</span>(<span class="str">"/users/:id"</span>, <span class="fn">get_user</span>)
  <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">delete</span>(<span class="str">"/users/:id"</span>, <span class="fn">delete_user</span>)`}</code
                    ></pre>

                <h3>Resource Routes</h3>
                <p>
                    Use <code>Router.resources</code> to generate all five RESTful
                    CRUD routes from a single call.
                </p>
                <pre><code
                        >{@html `<span class="kw">let</span> <span class="fn">app</span> <span class="op">=</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">new</span>()
  <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">resources</span>(<span class="str">"/users"</span>, {
    <span class="fn">index</span><span class="op">:</span>   <span class="fn">list_users</span>,
    <span class="fn">show</span><span class="op">:</span>    <span class="fn">get_user</span>,
    <span class="fn">create</span><span class="op">:</span>  <span class="fn">create_user</span>,
    <span class="fn">update</span><span class="op">:</span>  <span class="fn">update_user</span>,
    <span class="fn">destroy</span><span class="op">:</span> <span class="fn">delete_user</span>
  })`}</code
                    ></pre>
                <p>
                    This registers <code>GET /users</code>,
                    <code>GET /users/:id</code>,
                    <code>POST /users</code>, <code>PUT /users/:id</code>, and
                    <code>DELETE /users/:id</code>.
                </p>

                <h3>Route Groups</h3>
                <p>
                    Use <code>Router.group</code> to nest routes under a shared prefix.
                    This keeps related endpoints together and makes middleware scoping
                    easier.
                </p>
                <pre><code
                        >{@html `<span class="kw">let</span> <span class="fn">app</span> <span class="op">=</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">new</span>()
  <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">group</span>(<span class="str">"/api/v1"</span>, <span class="op">|</span><span class="fn">r</span><span class="op">|</span>
    <span class="fn">r</span>
    <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">get</span>(<span class="str">"/users"</span>, <span class="fn">list_users</span>)
    <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">post</span>(<span class="str">"/users"</span>, <span class="fn">create_user</span>))`}</code
                    ></pre>
            </section>

            <section id="requests">
                <h2>Reading Requests</h2>
                <p>
                    Handler functions receive a request value. Use
                    <code>Request</code> module functions to extract data from it.
                </p>
                <pre><code
                        >{@html `<span class="kw">fn</span> <span class="fn">handle</span>(<span class="fn">req</span><span class="op">:</span> <span class="type">Request</span>) <span class="op">-&gt;</span> <span class="type">Response</span> <span class="op">=</span>
  <span class="kw">let</span> <span class="fn">method</span> <span class="op">=</span> <span class="type">Request</span><span class="punct">.</span><span class="fn">method</span>(<span class="fn">req</span>)
  <span class="kw">let</span> <span class="fn">auth</span> <span class="op">=</span> <span class="type">Request</span><span class="punct">.</span><span class="fn">header</span>(<span class="fn">req</span>, <span class="str">"Authorization"</span>)
  <span class="kw">let</span> <span class="fn">body</span> <span class="op">=</span> <span class="type">Request</span><span class="punct">.</span><span class="fn">body_json</span>(<span class="fn">req</span>)
  <span class="type">Response</span><span class="punct">.</span><span class="fn">ok</span>(<span class="str">"Received \${method}"</span>)`}</code
                    ></pre>

                <h3>Record Enrichment</h3>
                <p>
                    Middleware can add typed fields directly to the request
                    using record update syntax. Handlers then access the data as
                    normal fields — no getters needed.
                </p>
                <pre><code
                        >{@html `<span class="comment">-- In middleware: enrich request after auth check</span>
<span class="kw">let</span> <span class="fn">req</span> <span class="op">=</span> { ..<span class="fn">req</span>, <span class="fn">user_id</span><span class="op">:</span> <span class="fn">user_id</span> }

<span class="comment">-- In handler: access it directly</span>
<span class="kw">let</span> <span class="fn">user_id</span> <span class="op">=</span> <span class="fn">req</span><span class="punct">.</span><span class="fn">user_id</span>`}</code
                    ></pre>
            </section>

            <section id="responses">
                <h2>Building Responses</h2>
                <p>
                    The <code>Response</code> module provides constructors for common
                    HTTP status codes and a builder pattern for headers.
                </p>
                <pre><code
                        >{@html `<span class="comment">-- JSON response</span>
<span class="type">Response</span><span class="punct">.</span><span class="fn">json</span>({ <span class="fn">name</span><span class="op">:</span> <span class="str">"Alice"</span>, <span class="fn">age</span><span class="op">:</span> <span class="num">30</span> })

<span class="comment">-- Custom status with headers</span>
<span class="type">Response</span><span class="punct">.</span><span class="fn">status</span>(<span class="num">201</span>, <span class="str">"Created"</span>)
  <span class="op">|&gt;</span> <span class="type">Response</span><span class="punct">.</span><span class="fn">with_header</span>(<span class="str">"Location"</span>, <span class="str">"/users/42"</span>)

<span class="comment">-- Redirect</span>
<span class="type">Response</span><span class="punct">.</span><span class="fn">redirect</span>(<span class="str">"/login"</span>)

<span class="comment">-- Error responses</span>
<span class="type">Response</span><span class="punct">.</span><span class="fn">not_found</span>(<span class="str">"User not found"</span>)
<span class="type">Response</span><span class="punct">.</span><span class="fn">bad_request</span>(<span class="str">"Missing name field"</span>)`}</code
                    ></pre>
            </section>

            <section id="middleware">
                <h2>Middleware</h2>
                <p>
                    Middleware wraps handlers to add cross-cutting behavior like
                    logging, authentication, or CORS headers. Use
                    <code>Router.use</code> to attach middleware to a router.
                </p>
                <pre><code
                        >{@html `<span class="kw">fn</span> <span class="fn">log_middleware</span>(<span class="fn">req</span>, <span class="fn">next</span>) <span class="op">=</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">println!</span>(<span class="str">"\${Request.method(req)} \${req.path}"</span>)
  <span class="fn">next</span>(<span class="fn">req</span>)

<span class="kw">let</span> <span class="fn">app</span> <span class="op">=</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">new</span>()
  <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">use</span>(<span class="fn">log_middleware</span>)
  <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">get</span>(<span class="str">"/"</span>, <span class="fn">home</span>)`}</code
                    ></pre>
            </section>

            <section id="example">
                <h2>Complete Example</h2>
                <p>
                    A minimal REST API with resource routing, JSON responses,
                    and error handling.
                </p>
                <pre><code
                        >{@html `<span class="annot">@prelude(server)</span>

<span class="kw">fn</span> <span class="fn">list_users</span>(<span class="fn">req</span>) <span class="op">=</span>
  <span class="type">Response</span><span class="punct">.</span><span class="fn">json</span>([{ <span class="fn">id</span><span class="op">:</span> <span class="num">1</span>, <span class="fn">name</span><span class="op">:</span> <span class="str">"Alice"</span> }])

<span class="kw">fn</span> <span class="fn">get_user</span>(<span class="fn">req</span>) <span class="op">=</span>
  <span class="kw">let</span> <span class="fn">id</span> <span class="op">=</span> <span class="type">Request</span><span class="punct">.</span><span class="fn">param</span>(<span class="fn">req</span>, <span class="str">"id"</span>)
  <span class="type">Response</span><span class="punct">.</span><span class="fn">json</span>({ <span class="fn">id</span><span class="op">:</span> <span class="fn">id</span>, <span class="fn">name</span><span class="op">:</span> <span class="str">"Alice"</span> })

<span class="kw">fn</span> <span class="fn">create_user</span>(<span class="fn">req</span>) <span class="op">=</span>
  <span class="type">Response</span><span class="punct">.</span><span class="fn">created</span>(<span class="str">"User created"</span>)

<span class="kw">fn</span> <span class="fn">update_user</span>(<span class="fn">req</span>) <span class="op">=</span>
  <span class="type">Response</span><span class="punct">.</span><span class="fn">ok</span>(<span class="str">"Updated"</span>)

<span class="kw">fn</span> <span class="fn">delete_user</span>(<span class="fn">req</span>) <span class="op">=</span>
  <span class="type">Response</span><span class="punct">.</span><span class="fn">no_content</span>()

<span class="kw">fn</span> <span class="fn">not_found_handler</span>(<span class="fn">req</span>) <span class="op">=</span>
  <span class="type">Response</span><span class="punct">.</span><span class="fn">not_found</span>(<span class="str">"Not found"</span>)

<span class="kw">fn</span> <span class="fn">main!</span>() <span class="op">-&gt;</span> <span class="effect">{Http}</span> <span class="type">Unit</span> <span class="op">=</span>
  <span class="kw">let</span> <span class="fn">app</span> <span class="op">=</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">new</span>()
    <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">resources</span>(<span class="str">"/users"</span>, {
      <span class="fn">index</span><span class="op">:</span>   <span class="fn">list_users</span>,
      <span class="fn">show</span><span class="op">:</span>    <span class="fn">get_user</span>,
      <span class="fn">create</span><span class="op">:</span>  <span class="fn">create_user</span>,
      <span class="fn">update</span><span class="op">:</span>  <span class="fn">update_user</span>,
      <span class="fn">destroy</span><span class="op">:</span> <span class="fn">delete_user</span>
    })
    <span class="op">|&gt;</span> <span class="type">Router</span><span class="punct">.</span><span class="fn">any</span>(<span class="str">"/*"</span>, <span class="fn">not_found_handler</span>)
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
                    <li><a href="#hello-world">Hello World</a></li>
                    <li>
                        <a href="#routing">Routing</a>
                        <ul>
                            <li><a href="#routing">Resource Routes</a></li>
                            <li><a href="#routing">Route Groups</a></li>
                        </ul>
                    </li>
                    <li><a href="#requests">Reading Requests</a></li>
                    <li><a href="#responses">Building Responses</a></li>
                    <li><a href="#middleware">Middleware</a></li>
                    <li><a href="#example">Complete Example</a></li>
                </ul>
            </nav>
        </aside>
    </div>
</main>
