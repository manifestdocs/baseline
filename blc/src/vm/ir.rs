use std::collections::HashMap;

use crate::analysis::types::Type;

// ---------------------------------------------------------------------------
// Tag Registry — compile-time integer IDs for enum tags
// ---------------------------------------------------------------------------

/// Maps enum tag strings to integer IDs for fast dispatch.
///
/// Pre-registers well-known tags (None=0, Some=1, Ok=2, Err=3).
/// Additional tags are assigned IDs as they are discovered during IR scanning.
#[derive(Debug, Clone)]
pub struct TagRegistry {
    tag_to_id: HashMap<String, u32>,
    id_to_tag: Vec<String>,
}

impl TagRegistry {
    /// Create a new registry with well-known tags pre-registered.
    pub fn new() -> Self {
        let well_known = ["None", "Some", "Ok", "Err"];
        let mut tag_to_id = HashMap::new();
        let mut id_to_tag = Vec::new();
        for (i, tag) in well_known.iter().enumerate() {
            tag_to_id.insert(tag.to_string(), i as u32);
            id_to_tag.push(tag.to_string());
        }
        TagRegistry { tag_to_id, id_to_tag }
    }

    /// Register a tag, returning its ID. Idempotent — returns existing ID if already registered.
    pub fn register(&mut self, tag: &str) -> u32 {
        if let Some(&id) = self.tag_to_id.get(tag) {
            return id;
        }
        let id = self.id_to_tag.len() as u32;
        self.tag_to_id.insert(tag.to_string(), id);
        self.id_to_tag.push(tag.to_string());
        id
    }

    /// Look up the integer ID for a tag, returning None if not registered.
    pub fn get_id(&self, tag: &str) -> Option<u32> {
        self.tag_to_id.get(tag).copied()
    }

    /// Look up the tag string for an ID.
    pub fn get_tag(&self, id: u32) -> Option<&str> {
        self.id_to_tag.get(id as usize).map(|s| s.as_str())
    }

    /// Number of registered tags.
    pub fn len(&self) -> usize {
        self.id_to_tag.len()
    }
}

impl Default for TagRegistry {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// IR Module & Function
// ---------------------------------------------------------------------------

/// A compiled module: a collection of named functions with an entry point.
#[derive(Debug, Clone)]
pub struct IrModule {
    pub functions: Vec<IrFunction>,
    /// Index of main/main! in `functions`.
    pub entry: usize,
    /// Compile-time enum tag → integer ID mapping.
    pub tags: TagRegistry,
}

/// A single function definition lowered from the CST.
#[derive(Debug, Clone)]
pub struct IrFunction {
    pub name: String,
    pub params: Vec<String>,
    pub body: Expr,
    /// Resolved function type (when type checker succeeded).
    pub ty: Option<Type>,
    pub span: Span,
}

/// Source location for diagnostics.
#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub line: usize,
    pub col: usize,
    pub start_byte: usize,
    pub end_byte: usize,
}

// ---------------------------------------------------------------------------
// Expressions
// ---------------------------------------------------------------------------

/// The core IR expression type. Lifetime-free, fully desugared.
/// Each node optionally carries a resolved `Type` from the type checker.
#[derive(Debug, Clone)]
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
    /// Call a known named function.
    CallDirect {
        name: String,
        args: Vec<Expr>,
        ty: Option<Type>,
    },
    /// Call a native/built-in function (e.g. Console.println!).
    CallNative {
        module: String,
        method: String,
        args: Vec<Expr>,
        ty: Option<Type>,
    },
    /// Call an indirect value (closure or function value on the stack).
    CallIndirect {
        callee: Box<Expr>,
        args: Vec<Expr>,
        ty: Option<Type>,
    },
    /// Tail-recursive self-call (reuses current frame).
    TailCall {
        name: String,
        args: Vec<Expr>,
        ty: Option<Type>,
    },
    /// Tail call through a closure/function value (uses return_call_indirect).
    TailCallIndirect {
        callee: Box<Expr>,
        args: Vec<Expr>,
        ty: Option<Type>,
    },

    // -- Constructors --
    /// Build an enum variant: `Some(42)` → `MakeEnum { tag: "Some", payload: Int(42) }`.
    MakeEnum {
        tag: String,
        payload: Box<Expr>,
        ty: Option<Type>,
    },
    /// Build a named struct: `User { id: 1, name: "Alice" }`.
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
    /// Record update: `{ ..base, field: newVal }`.
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
    /// Typed hole (??) — runtime error if reached.
    Hole,

    // -- Bindings --
    Let {
        pattern: Box<Pattern>,
        value: Box<Expr>,
        ty: Option<Type>,
    },
    Block(Vec<Expr>, Option<Type>),

    // -- Functions --
    Lambda {
        params: Vec<String>,
        body: Box<Expr>,
        ty: Option<Type>,
    },
    /// Closure object: captures + lifted function index.
    /// Produced by lambda lifting; consumed by JIT/codegen.
    MakeClosure {
        func_idx: usize,
        captures: Vec<Expr>,
    },
    /// Read a captured variable by index from the implicit closure parameter.
    /// Produced by lambda lifting inside lifted function bodies.
    GetClosureVar(usize),

    // -- Error handling --
    /// `expr?` — early return on Err/None, unwrap Ok/Some.
    Try {
        expr: Box<Expr>,
        ty: Option<Type>,
    },

    // -- String interpolation (desugared) --
    /// Concatenation of string segments (text + interpolated expressions).
    Concat(Vec<Expr>),

    // -- Effect handlers --
    /// `with { Effect: handler } body` — simple handler substitution.
    /// handlers: Vec<(effect_name, Vec<(method_name, handler_fn)>)>
    WithHandlers {
        handlers: Vec<(String, Vec<(String, Expr)>)>,
        body: Box<Expr>,
    },

    /// `handle body with { Effect.method!(args) -> handler_body }` — algebraic handler with resume.
    HandleEffect {
        body: Box<Expr>,
        clauses: Vec<HandlerClause>,
    },

    /// Perform a user-defined effect operation: `MyEffect.method!(args)`.
    PerformEffect {
        effect: String,
        method: String,
        args: Vec<Expr>,
        ty: Option<Type>,
    },

    // -- Test expressions --
    /// `expect <actual> <matcher>` — evaluates to Bool.
    Expect {
        actual: Box<Expr>,
        matcher: Box<Matcher>,
    },
}

// ---------------------------------------------------------------------------
// Operators
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    ListConcat,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}

// ---------------------------------------------------------------------------
// Pattern matching
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Expr,
}

/// A single clause in a `handle ... with { ... }` block.
#[derive(Debug, Clone)]
pub struct HandlerClause {
    pub effect: String,
    pub method: String,
    pub params: Vec<String>,
    pub body: Expr,
    /// If true, handler body is `resume(expr)` — no continuation capture needed.
    pub is_tail_resumptive: bool,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Var(String),
    Literal(Box<Expr>),
    Constructor(String, Vec<Pattern>),
    Tuple(Vec<Pattern>),
}

// ---------------------------------------------------------------------------
// Test matchers
// ---------------------------------------------------------------------------

/// Matcher for `expect` expressions in inline tests.
#[derive(Debug, Clone)]
pub enum Matcher {
    Equal(Box<Expr>),
    BeOk,
    BeSome,
    BeNone,
    BeEmpty,
    HaveLength(Box<Expr>),
    Contain(Box<Expr>),
    StartWith(Box<Expr>),
    Satisfy(Box<Expr>),
    Be(Pattern),
}

// ---------------------------------------------------------------------------
// Test module
// ---------------------------------------------------------------------------

/// A single inline test lowered from the CST.
#[derive(Debug, Clone)]
pub struct IrTest {
    pub name: String,
    pub function: Option<String>,
    pub body: Expr,
    pub line: usize,
    pub col: usize,
    pub end_line: usize,
    pub end_col: usize,
    pub skip: bool,
}

/// A module with both functions and inline tests.
#[derive(Debug, Clone)]
pub struct IrTestModule {
    pub functions: Vec<IrFunction>,
    pub tests: Vec<IrTest>,
    pub entry: usize,
    /// Compile-time enum tag → integer ID mapping.
    pub tags: TagRegistry,
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ir_types_construct() {
        // Verify basic IR node construction compiles and works
        let _e = Expr::Int(42);
        let _e = Expr::BinOp {
            op: BinOp::Add,
            lhs: Box::new(Expr::Int(1)),
            rhs: Box::new(Expr::Int(2)),
            ty: None,
        };
        let _e = Expr::CallDirect {
            name: "foo".into(),
            args: vec![Expr::Var("x".into(), None)],
            ty: None,
        };
        let _e = Expr::MakeEnum {
            tag: "Some".into(),
            payload: Box::new(Expr::Int(42)),
            ty: None,
        };
        let _p = Pattern::Constructor("Some".into(), vec![Pattern::Var("v".into())]);
    }

    #[test]
    fn ir_module_construct() {
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::Int(42),
                ty: None,
                span: Span {
                    line: 1,
                    col: 0,
                    start_byte: 0,
                    end_byte: 2,
                },
            }],
            entry: 0,
            tags: TagRegistry::new(),
        };
        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.entry, 0);
    }

    #[test]
    fn all_expr_variants_constructible() {
        // Ensure every Expr variant can be created
        let exprs: Vec<Expr> = vec![
            Expr::Int(0),
            Expr::Float(0.0),
            Expr::String("".into()),
            Expr::Bool(true),
            Expr::Unit,
            Expr::Var("x".into(), None),
            Expr::CallDirect {
                name: "f".into(),
                args: vec![],
                ty: None,
            },
            Expr::CallNative {
                module: "M".into(),
                method: "m".into(),
                args: vec![],
                ty: None,
            },
            Expr::CallIndirect {
                callee: Box::new(Expr::Var("f".into(), None)),
                args: vec![],
                ty: None,
            },
            Expr::TailCall {
                name: "f".into(),
                args: vec![],
                ty: None,
            },
            Expr::MakeEnum {
                tag: "T".into(),
                payload: Box::new(Expr::Unit),
                ty: None,
            },
            Expr::MakeStruct {
                name: "S".into(),
                fields: vec![],
                ty: None,
            },
            Expr::MakeList(vec![], None),
            Expr::MakeRecord(vec![], None),
            Expr::MakeTuple(vec![], None),
            Expr::MakeRange(Box::new(Expr::Int(0)), Box::new(Expr::Int(1))),
            Expr::UpdateRecord {
                base: Box::new(Expr::Unit),
                updates: vec![],
                ty: None,
            },
            Expr::GetField {
                object: Box::new(Expr::Unit),
                field: "f".into(),
                ty: None,
            },
            Expr::BinOp {
                op: BinOp::Add,
                lhs: Box::new(Expr::Int(0)),
                rhs: Box::new(Expr::Int(0)),
                ty: None,
            },
            Expr::UnaryOp {
                op: UnaryOp::Neg,
                operand: Box::new(Expr::Int(0)),
                ty: None,
            },
            Expr::And(Box::new(Expr::Bool(true)), Box::new(Expr::Bool(true))),
            Expr::Or(Box::new(Expr::Bool(true)), Box::new(Expr::Bool(false))),
            Expr::If {
                condition: Box::new(Expr::Bool(true)),
                then_branch: Box::new(Expr::Int(1)),
                else_branch: None,
                ty: None,
            },
            Expr::Match {
                subject: Box::new(Expr::Int(0)),
                arms: vec![],
                ty: None,
            },
            Expr::For {
                binding: "x".into(),
                iterable: Box::new(Expr::MakeList(vec![], None)),
                body: Box::new(Expr::Unit),
            },
            Expr::Let {
                pattern: Box::new(Pattern::Var("x".into())),
                value: Box::new(Expr::Int(0)),
                ty: None,
            },
            Expr::Block(vec![], None),
            Expr::Lambda {
                params: vec![],
                body: Box::new(Expr::Unit),
                ty: None,
            },
            Expr::Try {
                expr: Box::new(Expr::Unit),
                ty: None,
            },
            Expr::Concat(vec![]),
            Expr::WithHandlers {
                handlers: vec![],
                body: Box::new(Expr::Unit),
            },
            Expr::HandleEffect {
                body: Box::new(Expr::Unit),
                clauses: vec![],
            },
            Expr::PerformEffect {
                effect: "E".into(),
                method: "m".into(),
                args: vec![],
                ty: None,
            },
            Expr::Expect {
                actual: Box::new(Expr::Int(1)),
                matcher: Box::new(Matcher::Equal(Box::new(Expr::Int(1)))),
            },
            Expr::MakeClosure {
                func_idx: 0,
                captures: vec![Expr::Var("x".into(), None)],
            },
            Expr::GetClosureVar(0),
            Expr::TailCallIndirect {
                callee: Box::new(Expr::Var("f".into(), None)),
                args: vec![],
                ty: None,
            },
        ];
        assert_eq!(exprs.len(), 37);
    }

    #[test]
    fn all_pattern_variants_constructible() {
        let patterns: Vec<Pattern> = vec![
            Pattern::Wildcard,
            Pattern::Var("x".into()),
            Pattern::Literal(Box::new(Expr::Int(42))),
            Pattern::Constructor("Some".into(), vec![Pattern::Var("v".into())]),
            Pattern::Tuple(vec![Pattern::Var("a".into()), Pattern::Var("b".into())]),
        ];
        assert_eq!(patterns.len(), 5);
    }

    #[test]
    fn all_binop_variants() {
        let ops = [
            BinOp::Add,
            BinOp::Sub,
            BinOp::Mul,
            BinOp::Div,
            BinOp::Mod,
            BinOp::Eq,
            BinOp::Ne,
            BinOp::Lt,
            BinOp::Gt,
            BinOp::Le,
            BinOp::Ge,
        ];
        assert_eq!(ops.len(), 11);
    }

    #[test]
    fn all_unaryop_variants() {
        let ops = [UnaryOp::Neg, UnaryOp::Not];
        assert_eq!(ops.len(), 2);
    }

    #[test]
    fn tag_registry_well_known_tags() {
        let reg = TagRegistry::new();
        assert_eq!(reg.get_id("None"), Some(0));
        assert_eq!(reg.get_id("Some"), Some(1));
        assert_eq!(reg.get_id("Ok"), Some(2));
        assert_eq!(reg.get_id("Err"), Some(3));
        assert_eq!(reg.len(), 4);
    }

    #[test]
    fn tag_registry_register_new() {
        let mut reg = TagRegistry::new();
        let id = reg.register("MyTag");
        assert_eq!(id, 4);
        assert_eq!(reg.get_id("MyTag"), Some(4));
        assert_eq!(reg.get_tag(4), Some("MyTag"));
    }

    #[test]
    fn tag_registry_idempotent() {
        let mut reg = TagRegistry::new();
        let id1 = reg.register("Some");
        let id2 = reg.register("Some");
        assert_eq!(id1, id2);
        assert_eq!(id1, 1);
    }
}
