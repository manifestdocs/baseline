<script lang="ts">
	let { data } = $props();
</script>

<svelte:head>
	<title>Language Specification | Baseline</title>
	<meta name="description" content="The Baseline language specification: types, effects, modules, testing, and more." />
</svelte:head>

<main class="spec-page">
	<div class="spec-layout">
		<article class="spec-content">
			{@html data.html}
		</article>

		<aside class="spec-toc" aria-label="Table of contents">
			<h2 class="spec-toc-title">On this page</h2>
			<nav>
				<ul role="list">
					{#each data.toc as entry}
						<li class="toc-{entry.level}">
							<a href="#{entry.id}">{entry.text}</a>
						</li>
					{/each}
				</ul>
			</nav>
		</aside>
	</div>
</main>
