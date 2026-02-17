<svelte:head>
    <title>Guides | Baseline</title>
    <meta
        name="description"
        content="Practical guides for building with Baseline — web servers, databases, and more."
    />
</svelte:head>

<main id="main-content" class="guide-page">
    <div class="guide-index">
        <span class="overline">Guides</span>
        <h1>Practical Guides</h1>
        <p class="guide-index-intro">
            Step-by-step guides for building with Baseline's official modules.
            For function-level detail, see the
            <a href="/api">API Reference</a>.
        </p>

        <div class="guide-cards">
            <a href="/guides/web" class="guide-card">
                <h2>Web Framework</h2>
                <p>
                    Build HTTP servers with routing, middleware, JSON responses,
                    and error handling.
                </p>
                <span class="guide-card-modules"
                    >Router · Server · Request · Response · Middleware</span
                >
            </a>

            <a href="/guides/database" class="guide-card">
                <h2>Database</h2>
                <p>
                    Connect to Sqlite, Postgres, and Mysql with parameterized
                    queries.
                </p>
                <span class="guide-card-modules"
                    >Sqlite · Postgres · Mysql · Sql</span
                >
            </a>

            <a href="/guides/ai-tooling" class="guide-card">
                <h2>AI Agent Tooling</h2>
                <p>
                    Use structured diagnostics, built-in docs, and verification levels
                    to build effective AI coding workflows.
                </p>
                <span class="guide-card-modules"
                    >blc check · blc docs · blc test · blc cgp</span
                >
            </a>
        </div>
    </div>
</main>
