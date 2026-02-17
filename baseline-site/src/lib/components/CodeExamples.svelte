<script lang="ts">
	import { examples } from '$lib/data/examples';
</script>

<section id="examples" aria-labelledby="examples-heading">
	<span class="overline">Code</span>
	<h2 id="examples-heading">See it in action</h2>
	<div class="example-set">
		{#each examples as example}
			<div class="example-row">
				<div class="example-text">
					<h3>{example.title}</h3>
					<p>{@html example.description}</p>
				</div>
				<div class="example-code">
					<pre><code>{@html example.code}</code></pre>
				</div>
			</div>
		{/each}
	</div>
</section>
