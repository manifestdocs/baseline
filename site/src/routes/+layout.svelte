<script>
	import '../app.css';
	import Nav from '$lib/components/Nav.svelte';

	let { children } = $props();
</script>

<div class="site-layout">
	<Nav />
	{@render children()}
</div>
