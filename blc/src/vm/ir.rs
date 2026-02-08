use crate::analysis::types::Type;

// ---------------------------------------------------------------------------
// IR Module & Function
// ---------------------------------------------------------------------------

/// A compiled module: a collection of named functions with an entry point.
#[derive(Debug, Clone)]
pub struct IrModule {
    pub functions: Vec<IrFunction>,
    /// Index of main/main! in `functions`.
    pub entry: usize,
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
    /// `with { Effect: handler } body` or `handle body with { clauses }`.
    /// handlers: Vec<(effect_name, Vec<(method_name, handler_fn)>)>
    WithHandlers {
        handlers: Vec<(String, Vec<(String, Expr)>)>,
        body: Box<Expr>,
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

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Var(String),
    Literal(Box<Expr>),
    Constructor(String, Vec<Pattern>),
    Tuple(Vec<Pattern>),
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
        ];
        assert_eq!(exprs.len(), 31);
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
}
