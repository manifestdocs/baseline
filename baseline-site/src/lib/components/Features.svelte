<script lang="ts">
	import { features } from '$lib/data/features';
</script>

<section id="features" aria-labelledby="features-heading">
	<span class="overline">Features</span>
	<h2 id="features-heading">Language at a glance</h2>
	<div class="feature-grid">
		{#each features as feature}
			<div class="feature-item">
				<dl>
					<dt>{feature.title}</dt>
					<dd>{@html feature.description}</dd>
				</dl>
			</div>
		{/each}
	</div>
</section>
