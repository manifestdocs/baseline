<section id="benefits" aria-labelledby="benefits-heading">
	<span class="overline">Why Baseline</span>
	<h2 id="benefits-heading">Correct by construction</h2>
	<div class="benefits-grid">
		<div class="benefit">
			<h3>Effects are capabilities</h3>
			<p>
				A function typed <code>{'{Console, Http}'}</code> can print and make HTTP
				calls, and nothing else. Side effects are declared in type signatures
				and enforced by the compiler. No dependency can access the filesystem
				unless you grant it <code>{'{Fs}'}</code>. The default is no access.
			</p>
		</div>
		<div class="benefit">
			<h3>Refinement types</h3>
			<p>
				<code>type Port = Int where 1 &lt;= self &lt;= 65535</code> proves constraints at compile
				time, not at runtime. Define what valid data looks like once. The compiler
				checks every use. No defensive re-validation. No scattered checks.
			</p>
		</div>
		<div class="benefit">
			<h3>No null, no exceptions</h3>
			<p>
				<code>Option&lt;T&gt;</code> and <code>Result&lt;T, E&gt;</code> are the
				only error paths. Pattern matching is exhaustive &mdash; the compiler rejects
				code that leaves a branch unhandled. If a function can fail, its signature
				says so. If it returns a value, that value exists.
			</p>
		</div>
		<div class="benefit">
			<h3>One way to do each thing</h3>
			<p>
				One syntax for errors, one for effects, one for data transformation.
				Pipes over nesting. Combinators over loops. Fewer choices means fewer
				mistakes &mdash; and code that every team member and every LLM reads the same way.
			</p>
		</div>
	</div>
</section>
