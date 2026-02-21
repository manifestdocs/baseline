<script>
	import Hero from "$lib/components/Hero.svelte";
	import Benefits from "$lib/components/Benefits.svelte";
	import CodeExamples from "$lib/components/CodeExamples.svelte";
	import GetStarted from "$lib/components/GetStarted.svelte";
	import Cta from "$lib/components/Cta.svelte";
</script>

<svelte:head>
	<meta
		name="description"
		content="Baseline: a programming language with simple syntax, fast runtime, and compile-time safety."
	/>
</svelte:head>

<main id="main-content" class="site-main">
	<div class="container">
		<Hero />
		<GetStarted />
		<Benefits />
		<CodeExamples />
		<Cta />
	</div>
</main>
