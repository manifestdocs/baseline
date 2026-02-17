<script>
	import { page } from '$app/state';

	const guidePages = [
		{ href: '/guides/web', label: 'Web Framework' },
		{ href: '/guides/database', label: 'Database' },
		{ href: '/guides/ai-tooling', label: 'AI Agent Tooling' }
	];

	let onGuidesSection = $derived(page.url.pathname.startsWith('/guides'));
	let isHome = $derived(page.url.pathname === '/');
</script>

<nav class="site-nav" class:interior={!isHome} aria-label="Primary navigation">
	<div class="nav-header">
		<a href="/" class="wordmark" aria-label="Baseline home">Baseline</a>
	</div>
	<ul class="nav-links" role="list">
		<li><a href="/#benefits">Features</a></li>
		<li><a href="/#examples">Examples</a></li>
		<li><a href="/#start">Install</a></li>
		<li><a href="/spec">Spec</a></li>
		<li>
			<a href="/guides" class:active={onGuidesSection}>Guides</a>
			{#if onGuidesSection}
				<ul class="nav-sub" role="list">
					{#each guidePages as guide}
						<li>
							<a
								href={guide.href}
								class:active={page.url.pathname === guide.href}
							>{guide.label}</a>
						</li>
					{/each}
				</ul>
			{/if}
		</li>
		<li><a href="/api">API</a></li>
	</ul>
	<div class="nav-footer">
		<a
			href="https://github.com/manifestdocs/baseline"
			aria-label="GitHub (external site)">GitHub</a
		>
	</div>
</nav>
