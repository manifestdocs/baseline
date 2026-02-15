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

			<section class="syntax-guide">
				<h2>Reading Signatures</h2>
				<p class="api-intro">A few patterns appear throughout the standard library. Once you recognise them, every signature reads the same way.</p>
				<dl class="syntax-patterns">
					<div class="syntax-pattern">
						<dt><code>!</code> after a name</dt>
						<dd>The function has side effects. <code>Console.println!</code> writes to stdout; <code>Http.get!</code> makes a network call. Functions without <code>!</code> are pure.</dd>
					</div>
					<div class="syntax-pattern">
						<dt><code>{'{Effect}'}</code> in the return type</dt>
						<dd>Declares which effects a function uses. <code>fn main!() -&gt; {'{Console}'} Unit</code> means this function uses the Console and returns nothing. The compiler rejects undeclared effects.</dd>
					</div>
					<div class="syntax-pattern">
						<dt><code>Option&lt;T&gt;</code></dt>
						<dd>The value might not exist. Use <code>match</code> to handle <code>Some(value)</code> and <code>None</code>, or use <code>?</code> to propagate.</dd>
					</div>
					<div class="syntax-pattern">
						<dt><code>Result&lt;T, E&gt;</code></dt>
						<dd>The operation can fail. <code>Ok(value)</code> on success, <code>Err(error)</code> on failure. Use <code>?</code> to propagate errors or <code>match</code> to handle them explicitly.</dd>
					</div>
					<div class="syntax-pattern">
						<dt><code>|&gt;</code> (pipe)</dt>
						<dd>Passes the left-hand value as the first argument to the right-hand function. <code>list |&gt; List.map(f) |&gt; List.filter(g)</code> reads left to right.</dd>
					</div>
					<div class="syntax-pattern">
						<dt><code>@prelude(level)</code></dt>
						<dd>Controls how much of the standard library is auto-imported into your file. Think of it as choosing a starter kit. <code>none</code> gives you nothing, <code>script</code> gives you console I/O and common utilities, <code>server</code> adds networking and file access.</dd>
					</div>
				</dl>
			</section>

			{#each data.modules as mod}
				<section id={slugify(mod.name)} class="api-module">
					<h2>
						{mod.name}
						<span class="module-prelude">@prelude({mod.functions[0]?.prelude_level})</span>
					</h2>
					{#if mod.description}
						<p class="api-module-desc">{mod.description}</p>
					{/if}
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
			<h2 class="api-toc-title">On this page</h2>
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
