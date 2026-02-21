<script>
	import { page } from '$app/state';

	const guidePages = [
		{ href: '/guides/web', label: 'Web Framework' },
		{ href: '/guides/database', label: 'Database' },
		{ href: '/guides/ai-tooling', label: 'AI Agent Tooling' }
	];

	let onGuidesSection = $derived(page.url.pathname.startsWith('/guides'));
	let isHome = $derived(page.url.pathname === '/');
	let menuOpen = $state(false);

	function toggleMenu() {
		menuOpen = !menuOpen;
	}

	function closeMenu() {
		menuOpen = false;
	}
</script>

<nav class="site-nav" class:interior={!isHome} aria-label="Primary navigation">
	<div class="nav-header">
		<a href="/" class="wordmark" aria-label="Baseline home">Baseline</a>
		<button
			class="nav-toggle"
			onclick={toggleMenu}
			aria-expanded={menuOpen}
			aria-label={menuOpen ? 'Close menu' : 'Open menu'}
		>
			{#if menuOpen}✕{:else}☰{/if}
		</button>
	</div>
	<div class="nav-drawer" class:open={menuOpen}>
		<ul class="nav-links" role="list">
			<li><a href="/#start" onclick={closeMenu}>Install</a></li>
			<li><a href="/#benefits" onclick={closeMenu}>Features</a></li>
			<li><a href="/#examples" onclick={closeMenu}>Examples</a></li>
			<li><a href="/spec" onclick={closeMenu}>Spec</a></li>
			<li>
				<a href="/guides" class:active={onGuidesSection} onclick={closeMenu}>Guides</a>
				{#if onGuidesSection}
					<ul class="nav-sub" role="list">
						{#each guidePages as guide}
							<li>
								<a
									href={guide.href}
									class:active={page.url.pathname === guide.href}
									onclick={closeMenu}
								>{guide.label}</a>
							</li>
						{/each}
					</ul>
				{/if}
			</li>
			<li><a href="/api" onclick={closeMenu}>API</a></li>
		</ul>
		<div class="nav-footer">
			<a
				href="https://github.com/manifestdocs/baseline"
				aria-label="GitHub (external site)">GitHub</a
			>
			<a href="/llms.txt">llms.txt</a>
		</div>
	</div>
</nav>
