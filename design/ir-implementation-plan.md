# Baseline IR Implementation Plan

## Problem Statement

The VM compiler (`blc/src/vm/compiler.rs`, 3,087 lines) operates directly on tree-sitter CST nodes, mixing ~12 desugaring transformations with bytecode emission in a single pass. This creates three problems:

1. **Desugaring is interleaved with codegen** — pipe expansion, match compilation, try expansion, for-loop lowering, string interpolation, and constructor synthesis all happen during bytecode emission
2. **TypeMap is underutilized** — the type checker produces a full `HashMap<usize, Type>` but it's only used for integer specialization in binary ops
3. **No shared representation for future backends** — adding Cranelift JIT or C codegen would require reimplementing all desugaring logic

## Proposed Architecture

```
tree-sitter CST
      │
      ▼
┌─────────────┐     TypeMap from
│  IR Lowering │◄──  type checker
│  (lower.rs)  │
└──────┬──────┘
       │  Vec<IrFunction> + IrModule
       ▼
┌─────────────┐
│   Codegen    │     Bytecode VM compiler
│ (codegen.rs) │     (simple, ~800 lines)
└──────┬──────┘
       │  Program (Vec<Chunk>)
       ▼
┌─────────────┐
│   VM + Opts  │     Existing VM + peephole passes
│  (vm.rs)     │
└─────────────┘
```

## IR Design

### Core Types

The IR is a lifetime-free Rust enum tree. Each expression node carries an `Option<Type>` — present when the type checker resolved it, `None` for graceful degradation.

```rust
// blc/src/vm/ir.rs

pub struct IrModule {
    pub functions: Vec<IrFunction>,
    pub entry: usize,                    // index of main/main! in functions
}

pub struct IrFunction {
    pub name: String,
    pub params: Vec<String>,
    pub body: Expr,
    pub ty: Option<Type>,                // resolved function type
    pub span: Span,
}

pub struct Span {
    pub line: usize,
    pub col: usize,
    pub start_byte: usize,
    pub end_byte: usize,
}

pub enum Expr {
    // -- Literals --
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Unit,

    // -- Variables --
    Var(String, Option<Type>),

    // -- Calls (resolved, unambiguous) --
    CallDirect {
        name: String,                    // known function name
        args: Vec<Expr>,
        ty: Option<Type>,
    },
    CallNative {
        module: String,                  // e.g. "Console"
        method: String,                  // e.g. "println!"
        args: Vec<Expr>,
        ty: Option<Type>,
    },
    CallIndirect {
        callee: Box<Expr>,               // closure or function value
        args: Vec<Expr>,
        ty: Option<Type>,
    },
    TailCall {
        name: String,
        args: Vec<Expr>,
        ty: Option<Type>,
    },

    // -- Constructors (resolved from type_identifier/struct_expression) --
    MakeEnum {
        tag: String,
        payload: Box<Expr>,              // Unit for nullary
        ty: Option<Type>,
    },
    MakeStruct {
        name: String,
        fields: Vec<(String, Expr)>,
        ty: Option<Type>,
    },

    // -- Data construction --
    MakeList(Vec<Expr>, Option<Type>),
    MakeRecord(Vec<(String, Expr)>, Option<Type>),
    MakeTuple(Vec<Expr>, Option<Type>),
    MakeRange(Box<Expr>, Box<Expr>),
    UpdateRecord {
        base: Box<Expr>,
        updates: Vec<(String, Expr)>,
        ty: Option<Type>,
    },

    // -- Field access --
    GetField {
        object: Box<Expr>,
        field: String,
        ty: Option<Type>,
    },

    // -- Arithmetic & logic --
    BinOp {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        ty: Option<Type>,
    },
    UnaryOp {
        op: UnaryOp,
        operand: Box<Expr>,
        ty: Option<Type>,
    },

    // -- Short-circuit boolean --
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),

    // -- Control flow --
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
        ty: Option<Type>,
    },
    Match {
        subject: Box<Expr>,
        arms: Vec<MatchArm>,
        ty: Option<Type>,
    },
    For {
        binding: String,
        iterable: Box<Expr>,
        body: Box<Expr>,
    },

    // -- Bindings --
    Let {
        pattern: Pattern,
        value: Box<Expr>,
        ty: Option<Type>,
    },
    Block(Vec<Expr>, Option<Type>),

    // -- Functions --
    Lambda {
        params: Vec<String>,
        captures: Vec<String>,           // explicit capture list
        body: Box<Expr>,
        ty: Option<Type>,
    },

    // -- Error handling --
    Try {
        expr: Box<Expr>,
        ty: Option<Type>,               // inner type (T from Option<T>)
    },

    // -- String interpolation (desugared) --
    Concat(Vec<Expr>),
}

pub enum BinOp {
    Add, Sub, Mul, Div, Mod,
    Eq, Ne, Lt, Gt, Le, Ge,
}

pub enum UnaryOp {
    Neg,    // -
    Not,    // not
}

pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Expr,
}

pub enum Pattern {
    Wildcard,
    Var(String),
    Literal(Expr),                       // Int, Float, String, Bool
    Constructor(String, Vec<Pattern>),   // Some(x), Ok(v)
    Tuple(Vec<Pattern>),
}
```

### What the IR Resolves

The IR's primary job is to resolve ambiguity and desugar syntax. After lowering:

| CST ambiguity | IR resolution |
|---|---|
| `f(x)` — is `f` a function, closure, native, or constructor? | `CallDirect`, `CallNative`, `CallIndirect`, `TailCall`, or `MakeEnum` |
| `x \|> f(a)` — pipe syntax | `CallDirect/CallNative/CallIndirect` with `x` prepended to args |
| `Some(42)` — call or constructor? | `MakeEnum { tag: "Some", payload: Int(42) }` |
| `User { id: 1 }` — struct or record? | `MakeStruct` (has type_identifier) vs `MakeRecord` |
| `"hello ${n}"` — interpolation | `Concat([String("hello "), Var("n")])` |
| `n * factorial(n-1)` — is recursive call in tail position? | `CallDirect` (not TailCall — binary operands are never tail) |
| `x && y` — short-circuit or binary? | `And(x, y)` (not BinOp) |

### What the IR Does NOT Do (v1)

- No pattern compilation to decision trees (linear clause list, same as current)
- No effect representation (effects stay in the analysis layer)
- No import/module resolution (handled before IR lowering, same as today)
- No monomorphization or specialization (types are carried but not acted on during lowering)
- No closure upvalue index resolution (codegen handles slot assignment)

## Implementation Phases

### Phase 1: IR Types (~1 day)

**New file: `blc/src/vm/ir.rs`**

Define `Expr`, `Pattern`, `MatchArm`, `BinOp`, `UnaryOp`, `IrFunction`, `IrModule`, `Span`. Keep it simple — plain Rust enums/structs, no traits, no visitors yet.

Add `pub mod ir;` to `blc/src/vm/mod.rs`.

**Acceptance criteria:**
- [ ] IR types compile
- [ ] All current CST node kinds have a corresponding IR representation

### Phase 2: CST-to-IR Lowering (~3-4 days)

**New file: `blc/src/vm/lower.rs`**

Build a `Lowerer` struct that walks CST nodes and produces IR. This is where all desugaring happens.

```rust
pub struct Lowerer<'a> {
    source: &'a str,
    type_map: Option<TypeMap>,
    natives: &'a NativeRegistry,
    functions: HashSet<String>,          // known function names (for CallDirect)
    constructors: HashSet<String>,       // known enum constructors
    current_fn_name: Option<String>,     // for TCO detection
}

impl<'a> Lowerer<'a> {
    pub fn lower_module(&mut self, root: &Node) -> Result<IrModule, LowerError>;
    pub fn lower_expression(&mut self, node: &Node) -> Result<Expr, LowerError>;
    fn lower_call(&mut self, node: &Node) -> Result<Expr, LowerError>;
    fn lower_match(&mut self, node: &Node) -> Result<Expr, LowerError>;
    fn lower_pipe(&mut self, node: &Node) -> Result<Expr, LowerError>;
    fn lower_string_literal(&mut self, node: &Node) -> Result<Expr, LowerError>;
    fn lower_pattern(&mut self, node: &Node) -> Result<Pattern, LowerError>;
    // ... etc
}
```

**Key responsibilities:**
1. Resolve call targets (direct/native/indirect/constructor/tail)
2. Desugar pipes into calls
3. Desugar string interpolation into Concat
4. Desugar for-loops into IR For nodes
5. Extract closure capture lists (free variable analysis)
6. Attach types from TypeMap to IR nodes
7. Detect tail position for TCO
8. Constant folding (move `try_eval_const` here)

**Capture analysis approach:**
Walk the lambda body, collect free variables (not in params, not globals), produce explicit capture list. This replaces the current ad-hoc upvalue discovery in the bytecode compiler.

**Acceptance criteria:**
- [ ] Every CST expression kind that compiler.rs handles has a lowering path
- [ ] Unit tests: `lower("1 + 2")` produces `BinOp { Add, Int(1), Int(2) }`
- [ ] Unit tests: `lower("x |> f")` produces `CallDirect/CallIndirect { args: [Var("x")] }`
- [ ] Unit tests: `lower("Some(42)")` produces `MakeEnum { tag: "Some", payload: Int(42) }`
- [ ] Constant folding works at IR level: `lower("2 + 3")` produces `Int(5)`

### Phase 3: IR-to-Bytecode Codegen (~3-4 days)

**New file: `blc/src/vm/codegen.rs`**

Build a `Codegen` struct that walks IR and emits bytecode. This replaces the current `Compiler` but is much simpler because all desugaring is already done.

```rust
pub struct Codegen<'a> {
    chunk: Chunk,
    locals: Vec<Local>,
    scope_depth: usize,
    compiled_chunks: Vec<Chunk>,
    enclosing: Vec<CodegenFrame>,
    upvalues: Vec<Upvalue>,
    natives: &'a NativeRegistry,
    functions: HashMap<String, usize>,
}

impl<'a> Codegen<'a> {
    pub fn generate(module: &IrModule, natives: &NativeRegistry) -> Result<Program, CodegenError>;
    fn gen_expr(&mut self, expr: &Expr) -> Result<(), CodegenError>;
    fn gen_call_direct(&mut self, name: &str, args: &[Expr]) -> Result<(), CodegenError>;
    fn gen_match(&mut self, subject: &Expr, arms: &[MatchArm]) -> Result<(), CodegenError>;
    fn gen_lambda(&mut self, params: &[String], captures: &[String], body: &Expr) -> Result<(), CodegenError>;
    // ... etc
}
```

**Key simplifications over current compiler.rs:**
- No CST node kind string matching — just `match expr { Expr::Int(n) => ... }`
- No pipe desugaring, string interpolation, or constructor resolution — already done
- No TypeMap lookup — types are on the IR nodes
- No tail position tracking — TailCall is already a distinct IR variant
- Call kinds are pre-resolved — just emit the right opcode

**Estimated size: ~800 lines** (vs 3,087 for current compiler.rs)

**Acceptance criteria:**
- [ ] Can compile all IR node types to bytecode
- [ ] Type-directed specialization uses `Option<Type>` on IR nodes (not TypeMap lookup)
- [ ] Produces identical bytecode as the old compiler for the test suite

### Phase 4: Wire Up & Differential Testing (~2 days)

**Modify: `blc/src/main.rs`, `blc/src/vm/module_compiler.rs`**

Add a new compilation path: CST → lower → codegen → Program. Run it alongside the existing compiler path.

```rust
// In run_file_vm():
let ir_module = Lowerer::new(source, natives, type_map).lower_module(&root)?;
let program = Codegen::generate(&ir_module, &natives)?;
```

**Differential testing strategy:**
- For every test in the suite, run both old compiler and new IR pipeline
- Compare the `Program` output (chunk count, opcodes, constants)
- Compare runtime results (execute both, assert equal output)

**Wire up module compilation:**
- Update `compile_with_imports` to use the IR path for each module
- TypeMap now flows through module compilation (currently missing)

**Acceptance criteria:**
- [ ] All 439+ lib tests pass through the IR pipeline
- [ ] All e2e, conformance, differential, and property tests pass
- [ ] Benchmark: fib(35) within 5% of current performance (0.94s)

### Phase 5: Swap Default & Cleanup (~1 day)

- Make the IR pipeline the default (remove old `Compiler` struct)
- Rename `codegen.rs` to `compiler.rs` (or keep both names, with `compiler.rs` being the old one behind a flag)
- Delete the old `compile_expression` → CST dispatch code
- Update module_compiler to use IR path exclusively

**Acceptance criteria:**
- [ ] Old compiler code removed (or behind `--legacy-compiler` flag)
- [ ] All tests pass
- [ ] No performance regression

## File Plan

| File | Action | Lines (est.) |
|---|---|---|
| `blc/src/vm/ir.rs` | NEW | ~250 |
| `blc/src/vm/lower.rs` | NEW | ~900 |
| `blc/src/vm/codegen.rs` | NEW | ~800 |
| `blc/src/vm/mod.rs` | MODIFY | +3 (module declarations) |
| `blc/src/main.rs` | MODIFY | ~20 (wire new pipeline) |
| `blc/src/vm/module_compiler.rs` | MODIFY | ~30 (use IR path) |
| `blc/src/vm/compiler.rs` | DELETE (Phase 5) | -3,087 |

**Net change:** ~1,950 new lines replace ~3,087 old lines (37% reduction)

## Risks & Mitigations

| Risk | Mitigation |
|---|---|
| Subtle behavioral differences between old and new pipeline | Differential testing on every test case |
| Performance regression from extra IR allocation pass | IR nodes are small; the overhead is dominated by VM dispatch. Benchmark at Phase 4. |
| Capture analysis may disagree with current upvalue resolution | Unit test specific closure patterns; compare captured variable sets |
| Module compilation divergence (no TypeMap in current path) | IR path fixes this — TypeMap flows through module lowering |
| Scope creep — temptation to add optimizations during migration | Strict rule: Phase 1-4 produce identical behavior. Optimizations come later. |

## Future Work (Post-Migration)

Once the IR is in place, these become straightforward:

- **IR-level constant propagation** — fold known variables, not just literals
- **Dead code elimination** — remove unreachable match arms, unused bindings
- **Pattern match compilation** — decision trees for nested patterns
- **Inlining** — small functions inlined at IR level
- **Unboxing** — `Int`-typed IR nodes skip NaN-boxing in a future native backend
- **Cranelift JIT** — codegen from IR to native, sharing all desugaring with bytecode path
- **Effect lowering** — when v0.2 adds custom handlers, the IR can represent evidence-passing

## References

- ZINC report (Leroy 1990) — OCaml's Lambda IR design
- Koka Core / lambda-k — explicitly typed System F with effects
- Roc Canonical IR — type-annotated, progressively lowered
- `memory/sister-languages-research.md` — comparative analysis
