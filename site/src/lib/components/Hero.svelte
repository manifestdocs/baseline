<section class="hero" aria-labelledby="hero-heading">
	<h1 id="hero-heading" class="wordmark">Baseline</h1>
	<div class="hero-split">
		<div class="hero-left">
			<p class="hero-subtitle">
				Simple syntax. Fast runtime. Catches mistakes. Baseline is easy
				to learn and hard to break, whether you're writing the code
				yourself, with your team, or an AI agent.
			</p>
			<ul class="hero-pillars" role="list">
				<li class="hero-pillar">
					<em>Simple to learn</em>
					<span
						>Small, consistent syntax that reads the same everywhere</span
					>
				</li>
				<li class="hero-pillar">
					<em>Fast and lightweight</em>
					<span>Compiled to native code with low memory usage</span>
				</li>
				<li class="hero-pillar">
					<em>Safe by default</em>
					<span
						>The compiler checks types, effects, and error handling
						before your code runs</span
					>
				</li>
			</ul>
		</div>
		<div class="hero-right">
			<pre
				aria-label="Baseline code example: fetching a user with tracked effects"><code
					>{@html `<span class="kw">fn</span> <span class="fn">fetch_user!</span><span class="punct">(</span><span class="fn">id</span><span class="op">:</span> <span class="type">Int</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="effect">{Http, Console}</span> <span class="type">Result&lt;String, String&gt;</span> <span class="op">=</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">print!</span><span class="punct">(</span><span class="str">"Fetching user \${id}"</span><span class="punct">)</span>
  <span class="kw">let</span> <span class="fn">response</span> <span class="op">=</span> <span class="type">Http</span><span class="punct">.</span><span class="fn">get!</span><span class="punct">(</span><span class="str">"/users/\${id}"</span><span class="punct">)</span><span class="op">?</span>
  <span class="kw">match</span> <span class="fn">response</span><span class="punct">.</span><span class="fn">status</span>
    <span class="num">200</span> <span class="op">-&gt;</span> <span class="type">Ok</span><span class="punct">(</span><span class="fn">response</span><span class="punct">.</span><span class="fn">body</span><span class="punct">)</span>
    <span class="num">404</span> <span class="op">-&gt;</span> <span class="type">Err</span><span class="punct">(</span><span class="str">"User not found"</span><span class="punct">)</span>
    <span class="fn">code</span> <span class="op">-&gt;</span> <span class="type">Err</span><span class="punct">(</span><span class="str">"HTTP \${code}"</span><span class="punct">)</span>`}</code
				></pre>
			<div class="btn-group" style="margin-top: var(--sp-4);">
				<a href="#start" class="button button-primary">Get Started</a>
				<a
					href="https://github.com/manifestdocs/baseline"
					class="button"
					aria-label="View on GitHub (external site)"
					>View on GitHub</a
				>
			</div>
		</div>
	</div>
</section>
