<section id="benefits" aria-labelledby="benefits-heading">
	<span class="overline">Why Baseline</span>
	<h2 id="benefits-heading">Catch bugs in the compiler, not in production</h2>
	<p class="section-intro">
		Dozens of languages, decades of practice, and the same bugs keep getting shipped.
		Type errors. No method errors. Undefined. Null is not a function.
		Baseline's compiler rejects them before your code runs.
	</p>
	<div class="benefits-grid">
		<div class="benefit">
			<h3>No hidden side effects</h3>
			<p>
				Ever had a dependency silently write to disk or phone home?
				In Baseline, if a function doesn't declare <code>{'{Fs}'}</code> in its type,
				it physically cannot touch the filesystem. Side effects are
				<em>capabilities</em>, visible in every signature, enforced by the compiler.
				You see the blast radius before you call.
			</p>
		</div>
		<div class="benefit">
			<h3>Catch invalid data at compile time</h3>
			<p>
				Port numbers, positive IDs, bounded ranges: stop writing runtime checks
				that you hope every caller remembers.
				<code>type Port = Int where 1 &lt;= self &lt;= 65535</code> is proved once
				and enforced everywhere. If bad data reaches your function, the compiler
				already rejected the program.
			</p>
		</div>
		<div class="benefit">
			<h3>Every failure path is visible</h3>
			<p>
				No <code>undefined is not a function</code>. No uncaught exception
				crashing production at 2 AM. If a function can fail, its return type
				says <code>Result&lt;T, E&gt;</code> and the compiler forces you to handle
				both cases. Null doesn't exist. Exceptions don't exist.
			</p>
		</div>
		<div class="benefit">
			<h3>Code that reads the same everywhere</h3>
			<p>
				One syntax for errors. One for effects. One for data pipelines.
				Whether a teammate or an AI agent wrote it, you can read it
				instantly. Fewer conventions to argue about, fewer ways for
				style drift to hide bugs.
			</p>
		</div>
	</div>
</section>
