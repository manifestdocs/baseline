<script lang="ts">
    let { data } = $props();
    let query = $state("");

    function slugify(name: string): string {
        return name.toLowerCase().replace(/[^a-z0-9]+/g, "-");
    }

    let filteredModules = $derived.by(() => {
        const q = query.trim().toLowerCase();
        if (!q) return data.modules;

        return data.modules
            .map((mod: any) => {
                const matchingFns = mod.functions.filter((func: any) => {
                    const fullName = `${mod.name}.${func.name}`.toLowerCase();
                    return (
                        fullName.includes(q) ||
                        func.signature.toLowerCase().includes(q) ||
                        (func.description &&
                            func.description.toLowerCase().includes(q)) ||
                        (func.example &&
                            func.example.toLowerCase().includes(q)) ||
                        func.effects.some((e: string) =>
                            e.toLowerCase().includes(q),
                        )
                    );
                });
                return { ...mod, functions: matchingFns };
            })
            .filter((mod: any) => mod.functions.length > 0);
    });

    let totalMatches = $derived(
        filteredModules.reduce(
            (sum: number, mod: any) => sum + mod.functions.length,
            0,
        ),
    );

    let isSearching = $derived(query.trim().length > 0);
</script>

<svelte:head>
    <title>{data.title} API | Baseline</title>
    <meta name="description" content={data.description} />
</svelte:head>

<main id="main-content" class="api-page">
    <div class="api-layout">
        <article class="api-content">
            <p class="api-breadcrumb"><a href="/api">API Reference</a> →</p>
            <h1>{data.title}</h1>
            <p class="api-intro">
                {data.description}
            </p>

            {#if data.guide}
                <p class="api-intro">
                    <a href={data.guide}>{data.title} Guide →</a>
                </p>
            {/if}

            <div class="api-search">
                <input
                    type="text"
                    bind:value={query}
                    placeholder="Search {data.title.toLowerCase()} functions…"
                    class="api-search-input"
                    aria-label="Filter {data.title} API reference"
                />
                {#if query}
                    <button
                        class="api-search-clear"
                        onclick={() => (query = "")}
                        aria-label="Clear search">✕</button
                    >
                {/if}
            </div>

            {#if isSearching}
                <p class="api-result-count">
                    {totalMatches} function{totalMatches === 1 ? "" : "s"} matching
                    "{query}"
                </p>
            {/if}

            {#if isSearching && filteredModules.length === 0}
                <p class="api-no-results">
                    No functions matching <strong>"{query}"</strong>
                </p>
            {/if}

            {#each filteredModules as mod}
                <section id={slugify(mod.name)} class="api-module">
                    <h2>
                        {mod.name}
                        <span class="module-prelude"
                            >@prelude({mod.functions[0]?.prelude_level})</span
                        >
                    </h2>
                    {#if mod.description}
                        <p class="api-module-desc">
                            {mod.description}
                        </p>
                    {/if}
                    <div class="api-functions">
                        {#each mod.functions as func}
                            <div
                                class="api-fn"
                                id={slugify(mod.name + "-" + func.name)}
                            >
                                <div class="api-fn-name">
                                    <code>{mod.name}.{func.name}</code>
                                    {#if func.effects.length > 0}
                                        <span class="api-fn-effects">
                                            {#each func.effects as effect}
                                                <span class="tag tag-effect"
                                                    >{effect}</span
                                                >
                                            {/each}
                                        </span>
                                    {/if}
                                </div>
                                <pre><code>{func.signature}</code></pre>
                                {#if func.description}
                                    <p class="api-fn-desc">
                                        {func.description}
                                    </p>
                                {/if}
                                {#if func.example}
                                    <pre class="api-fn-example"><code
                                            >{func.example}</code
                                        ></pre>
                                {/if}
                            </div>
                        {/each}
                    </div>
                </section>
            {/each}

            <p class="guide-back">
                <a href="/api">← All Packages</a>
            </p>
        </article>

        <aside class="api-toc" aria-label="Module navigation">
            <h2 class="api-toc-title">Modules</h2>
            <nav>
                <ul role="list">
                    {#each filteredModules as mod}
                        <li>
                            <a href="#{slugify(mod.name)}">{mod.name}</a>
                            <span class="toc-count">{mod.functions.length}</span
                            >
                        </li>
                    {/each}
                </ul>
            </nav>
        </aside>
    </div>
</main>
