<script>
	import "../app.css";
	import Nav from "$lib/components/Nav.svelte";

	let { children } = $props();
</script>

<div class="site-layout">
	<a href="#main-content" class="skip-link">Skip to content</a>
	<Nav />
	{@render children()}
</div>
