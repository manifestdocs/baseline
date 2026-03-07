<script lang="ts">
    import { examples } from '$lib/data/examples';
</script>

<svelte:head>
    <title>Code Examples | Baseline</title>
    <meta
        name="description"
        content="Baseline code examples: pattern matching, pipes, effects, inline tests, and more."
    />
    <meta property="og:title" content="Code Examples | Baseline" />
    <meta property="og:description" content="Baseline code examples: pattern matching, pipes, effects, inline tests, and more." />
    <meta name="twitter:title" content="Code Examples | Baseline" />
    <meta name="twitter:description" content="Baseline code examples: pattern matching, pipes, effects, inline tests, and more." />
</svelte:head>

<main id="main-content" class="pt-[var(--site-top)] pr-8 pb-20 pl-[var(--content-left)]">
    <div class="max-w-[42em]">
        <h1 class="interior-h1 mb-5">Examples</h1>
        <p class="text-[var(--fg-dim)] mb-5 leading-relaxed">
            Baseline in action: complete, runnable programs covering
            pattern matching, data pipelines, tracked effects, web servers,
            databases, and more. Each example is a self-contained script you
            can save and run with <code>blc run</code>.
        </p>

        {#each examples as example}
            <section id={example.title.toLowerCase().replace(/\s+/g, '-')}>
                <h3>{example.title}</h3>
                <p>{@html example.description}</p>
                <pre><code>{@html example.code}</code></pre>
            </section>
        {/each}

        <h2 class="mt-12">Full examples</h2>
        <p class="text-[var(--fg-dim)] mb-5 leading-relaxed">
            Each example below is a complete, runnable program with
            explanation. Save and run with <code>blc run</code>.
        </p>

        <div class="guide-cards">
            <a href="/examples/hello-world" class="guide-card">
                <h2>Hello World</h2>
                <p>
                    Basic I/O, string interpolation, and your first
                    Baseline program.
                </p>
                <span class="guide-card-modules">Console · String · main!</span>
            </a>

            <a href="/examples/pattern-matching" class="guide-card">
                <h2>Pattern Matching</h2>
                <p>
                    Sum types, exhaustive matching, and constructor
                    destructuring.
                </p>
                <span class="guide-card-modules">type · match · Option · Result</span>
            </a>

            <a href="/examples/data-pipeline" class="guide-card">
                <h2>Data Pipeline</h2>
                <p>
                    List processing with pipes, map, filter, fold,
                    and function composition.
                </p>
                <span class="guide-card-modules">List · |> · map · filter · fold</span>
            </a>

            <a href="/examples/error-handling" class="guide-card">
                <h2>Error Handling</h2>
                <p>
                    Result and Option types, the ? operator, and
                    composing fallible operations.
                </p>
                <span class="guide-card-modules">Result · Option · ? · match</span>
            </a>

            <a href="/examples/effects" class="guide-card">
                <h2>Effects</h2>
                <p>
                    Tracked side effects, effect annotations, and
                    the effect system.
                </p>
                <span class="guide-card-modules">{"{Console}"} · {"{Http}"} · {"{Fs}"} · fn!</span>
            </a>

            <a href="/examples/refinement-types" class="guide-card">
                <h2>Refinement Types</h2>
                <p>
                    Compile-time validation with integer constraints
                    and the where clause.
                </p>
                <span class="guide-card-modules">type · where · Int · self</span>
            </a>

            <a href="/examples/web-server" class="guide-card">
                <h2>Web Server</h2>
                <p>
                    A complete HTTP server with routing, middleware,
                    and JSON responses.
                </p>
                <span class="guide-card-modules">Router · Server · Request · Response</span>
            </a>

            <a href="/examples/database" class="guide-card">
                <h2>Database</h2>
                <p>
                    Database queries with Sqlite, parameterized queries,
                    and the web framework.
                </p>
                <span class="guide-card-modules">Sqlite · Sql · query! · execute!</span>
            </a>

            <a href="/examples/testing" class="guide-card">
                <h2>Testing</h2>
                <p>
                    Inline tests with @test blocks, test organization,
                    and running tests.
                </p>
                <span class="guide-card-modules">@test · test · blc test</span>
            </a>
        </div>
    </div>
</main>
