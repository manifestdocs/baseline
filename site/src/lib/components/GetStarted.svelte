<section id="start" aria-labelledby="start-heading">
	<span class="overline">Get Started</span>
	<h2 id="start-heading">Up and running</h2>
	<div class="install-steps">

		<div class="row">
			<div class="one column">
				<div class="step-number" aria-hidden="true">1</div>
			</div>
			<div class="eleven columns">
				<div class="step-content">
					<h3>Install</h3>
					<pre><code>brew install baseline-lang/tap/blc</code></pre>
				</div>
			</div>
		</div>

		<div class="row">
			<div class="one column">
				<div class="step-number" aria-hidden="true">2</div>
			</div>
			<div class="eleven columns">
				<div class="step-content">
					<h3>Write</h3>
					<p>Create <code>hello.bl</code>:</p>
					<pre><code>{@html `<span class="annot">@prelude</span><span class="punct">(</span><span class="fn">script</span><span class="punct">)</span>

<span class="kw">fn</span> <span class="fn">greet</span><span class="punct">(</span><span class="fn">name</span><span class="op">:</span> <span class="type">String</span><span class="punct">)</span> <span class="op">-&gt;</span> <span class="type">String</span> <span class="op">=</span>
  <span class="str">"Hello, \${name}!"</span>

<span class="kw">fn</span> <span class="fn">main!</span><span class="punct">()</span> <span class="op">-&gt;</span> <span class="effect">{Console}</span> <span class="punct">()</span> <span class="op">=</span>
  <span class="kw">let</span> <span class="fn">message</span> <span class="op">=</span> <span class="fn">greet</span><span class="punct">(</span><span class="str">"World"</span><span class="punct">)</span>
  <span class="type">Console</span><span class="punct">.</span><span class="fn">print!</span><span class="punct">(</span><span class="fn">message</span><span class="punct">)</span>`}</code></pre>
				</div>
			</div>
		</div>

		<div class="row">
			<div class="one column">
				<div class="step-number" aria-hidden="true">3</div>
			</div>
			<div class="eleven columns">
				<div class="step-content">
					<h3>Run</h3>
					<pre><code>{@html `blc run hello.bl
<span class="comment"># Hello, World!</span>

blc check hello.bl --json
<span class="comment"># {"status": "ok", "diagnostics": []}</span>`}</code></pre>
				</div>
			</div>
		</div>

	</div>
</section>
