<script lang="ts">
	let { data } = $props();

	function slugify(name: string): string {
		return name.toLowerCase().replace(/[^a-z0-9]+/g, '-');
	}
</script>

<svelte:head>
	<title>API Reference | Baseline</title>
	<meta name="description" content="Baseline standard library API reference: all modules, functions, types, and effects." />
</svelte:head>

<main class="api-page">
	<div class="api-layout">
		<article class="api-content">
			<h1>Standard Library</h1>
			<p class="api-intro">
				All functions available through the <code>@prelude</code> system.
				Each function shows its type signature, required effects, and minimum prelude level.
			</p>

			{#each data.modules as mod}
				<section id={slugify(mod.name)} class="api-module">
					<h2>
						{mod.name}
						<span class="module-prelude">@prelude({mod.functions[0]?.prelude_level})</span>
					</h2>
					<div class="api-functions">
						{#each mod.functions as func}
							<div class="api-fn" id={slugify(mod.name + '-' + func.name)}>
								<div class="api-fn-name">
									<code>{mod.name}.{func.name}</code>
									{#if func.effects.length > 0}
										<span class="api-fn-effects">
											{#each func.effects as effect}
												<span class="tag tag-effect">{effect}</span>
											{/each}
										</span>
									{/if}
								</div>
								<pre><code>{func.signature}</code></pre>
								{#if func.description}
									<p class="api-fn-desc">{func.description}</p>
								{/if}
							</div>
						{/each}
					</div>
				</section>
			{/each}
		</article>

		<aside class="api-toc" aria-label="Module navigation">
			<h2 class="api-toc-title">Modules</h2>
			<nav>
				<ul role="list">
					{#each data.modules as mod}
						<li>
							<a href="#{slugify(mod.name)}">{mod.name}</a>
							<span class="toc-count">{mod.functions.length}</span>
						</li>
					{/each}
				</ul>
			</nav>
		</aside>
	</div>
</main>
