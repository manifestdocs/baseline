use std::collections::HashMap;

use crate::analysis::types::Type;

use super::chunk::{Chunk, CompileError, Op, Program};
use super::ir::*;
use super::natives::NativeRegistry;
use super::nvalue::NValue;

// ---------------------------------------------------------------------------
// Local / Upvalue / Frame (same semantics as the old compiler)
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
struct Local {
    name: String,
    depth: usize,
}

#[derive(Debug, Clone)]
struct Upvalue {
    name: String,
    is_local: bool,
    index: u16,
}

struct CodegenFrame {
    chunk: Chunk,
    locals: Vec<Local>,
    scope_depth: usize,
    upvalues: Vec<Upvalue>,
}

// ---------------------------------------------------------------------------
// Codegen
// ---------------------------------------------------------------------------

/// Generates bytecode from an IR module.
pub struct Codegen<'a> {
    chunk: Chunk,
    locals: Vec<Local>,
    scope_depth: usize,
    compiled_chunks: Vec<Chunk>,
    enclosing: Vec<CodegenFrame>,
    upvalues: Vec<Upvalue>,
    natives: &'a NativeRegistry,
    /// Function name → chunk index.
    functions: HashMap<String, usize>,
}

impl<'a> Codegen<'a> {
    pub fn new(natives: &'a NativeRegistry) -> Self {
        Codegen {
            chunk: Chunk::new(),
            locals: Vec::new(),
            scope_depth: 0,
            compiled_chunks: Vec::new(),
            enclosing: Vec::new(),
            upvalues: Vec::new(),
            natives,
            functions: HashMap::new(),
        }
    }

    /// Pre-populate with already-compiled chunks and function mappings (for imports).
    pub fn set_pre_compiled(&mut self, chunks: Vec<Chunk>, functions: HashMap<String, usize>) {
        self.compiled_chunks = chunks;
        self.functions = functions;
    }

    // -----------------------------------------------------------------------
    // Module-level generation
    // -----------------------------------------------------------------------

    /// Generate a complete Program from an IrModule.
    pub fn generate_program(mut self, module: &IrModule) -> Result<Program, CompileError> {
        let chunk_offset = self.compiled_chunks.len();

        // First pass: register all function names with chunk indices
        for (i, func) in module.functions.iter().enumerate() {
            self.functions.insert(func.name.clone(), chunk_offset + i);
        }

        // Pre-allocate chunk slots
        for _ in 0..module.functions.len() {
            self.compiled_chunks.push(Chunk::new());
        }

        // Second pass: compile each function body
        for (i, func) in module.functions.iter().enumerate() {
            self.chunk = Chunk::new();
            self.locals.clear();
            self.scope_depth = 0;

            // Declare parameters as locals
            for param in &func.params {
                self.declare_local(param);
            }

            self.gen_expr(&func.body, &func.span)?;
            let chunk = self.finish_chunk();
            self.compiled_chunks[chunk_offset + i] = chunk;
        }

        Ok(Program {
            chunks: self.compiled_chunks,
            entry: chunk_offset + module.entry,
        })
    }

    /// Generate chunks for a module (no entry point). Returns chunks and name→index map.
    /// If `set_pre_compiled` was called first, the module's functions are appended after
    /// existing chunks and can reference pre-compiled functions by name.
    pub fn generate_module(
        mut self,
        functions: &[IrFunction],
    ) -> Result<(Vec<Chunk>, HashMap<String, usize>), CompileError> {
        let chunk_offset = self.compiled_chunks.len();

        // First pass: register names (offset by pre-compiled chunks)
        for (i, func) in functions.iter().enumerate() {
            self.functions.insert(func.name.clone(), chunk_offset + i);
        }

        // Pre-allocate
        for _ in 0..functions.len() {
            self.compiled_chunks.push(Chunk::new());
        }

        // Compile each
        for (i, func) in functions.iter().enumerate() {
            self.chunk = Chunk::new();
            self.locals.clear();
            self.scope_depth = 0;

            for param in &func.params {
                self.declare_local(param);
            }

            self.gen_expr(&func.body, &func.span)?;
            let chunk = self.finish_chunk();
            self.compiled_chunks[chunk_offset + i] = chunk;
        }

        Ok((self.compiled_chunks, self.functions))
    }

    // -----------------------------------------------------------------------
    // Expression codegen
    // -----------------------------------------------------------------------

    fn gen_expr(&mut self, expr: &Expr, span: &Span) -> Result<(), CompileError> {
        match expr {
            Expr::Int(n) => {
                if *n >= i16::MIN as i64 && *n <= i16::MAX as i64 {
                    self.emit(Op::LoadSmallInt(*n as i16), span);
                } else {
                    let idx = self.chunk.add_constant(NValue::int(*n));
                    self.emit(Op::LoadConst(idx), span);
                }
            }
            Expr::Float(f) => {
                let idx = self.chunk.add_constant(NValue::float(*f));
                self.emit(Op::LoadConst(idx), span);
            }
            Expr::String(s) => {
                let idx = self.chunk.add_constant(NValue::string(s.as_str().into()));
                self.emit(Op::LoadConst(idx), span);
            }
            Expr::Bool(b) => {
                let idx = self.chunk.add_constant(NValue::bool(*b));
                self.emit(Op::LoadConst(idx), span);
            }
            Expr::Unit => {
                let idx = self.chunk.add_constant(NValue::unit());
                self.emit(Op::LoadConst(idx), span);
            }

            Expr::Var(name, _ty) => {
                self.gen_var(name, span)?;
            }

            Expr::CallDirect { name, args, .. } => {
                self.gen_call_direct(name, args, span)?;
            }
            Expr::CallNative {
                module,
                method,
                args,
                ..
            } => {
                self.gen_call_native(module, method, args, span)?;
            }
            Expr::CallIndirect { callee, args, .. }
            | Expr::TailCallIndirect { callee, args, .. } => {
                self.gen_expr(callee, span)?;
                for arg in args {
                    self.gen_expr(arg, span)?;
                }
                let argc = Self::checked_u8(args.len(), "arguments", span)?;
                self.emit(Op::Call(argc), span);
            }
            Expr::TailCall { args, .. } => {
                for arg in args {
                    self.gen_expr(arg, span)?;
                }
                let argc = Self::checked_u8(args.len(), "arguments", span)?;
                self.emit(Op::TailCall(argc), span);
            }

            Expr::MakeEnum { tag, payload, .. } => {
                self.gen_expr(payload, span)?;
                let tag_idx = self.chunk.add_constant(NValue::string(tag.as_str().into()));
                self.emit(Op::MakeEnum(tag_idx), span);
            }
            Expr::MakeStruct { name, fields, .. } => {
                // Pre-evaluate values into temp locals so untracked key
                // constants don't skew slot indices during gen_expr
                // (same fix as MakeRecord — see BASEL-223).
                self.begin_scope();
                let temps: Vec<String> = (0..fields.len())
                    .map(|i| format!("__struct_{}", i))
                    .collect();
                for (i, (_, val)) in fields.iter().enumerate() {
                    self.gen_expr(val, span)?;
                    self.declare_local(&temps[i]);
                }
                for (i, (key, _)) in fields.iter().enumerate() {
                    let key_idx = self.chunk.add_constant(NValue::string(key.as_str().into()));
                    self.emit(Op::LoadConst(key_idx), span);
                    self.gen_var(&temps[i], span)?;
                }
                let count = Self::checked_u16(fields.len(), "struct fields", span)?;
                self.emit(Op::MakeRecord(count), span);
                let tag_idx = self.chunk.add_constant(NValue::string(name.as_str().into()));
                self.emit(Op::MakeStruct(tag_idx), span);
                self.end_scope(span);
            }

            Expr::MakeList(elems, _) => {
                for elem in elems {
                    self.gen_expr(elem, span)?;
                }
                let count = Self::checked_u16(elems.len(), "list elements", span)?;
                self.emit(Op::MakeList(count), span);
            }
            Expr::MakeRecord(fields, _) => {
                // Pre-evaluate values into temp locals so untracked key
                // constants don't skew slot indices during gen_expr.
                // Without this, string interpolation inside record values
                // resolves GetLocal to the wrong stack position (BASEL-223).
                self.begin_scope();
                let temps: Vec<String> = (0..fields.len())
                    .map(|i| format!("__rec_{}", i))
                    .collect();
                for (i, (_, val)) in fields.iter().enumerate() {
                    self.gen_expr(val, span)?;
                    self.declare_local(&temps[i]);
                }
                for (i, (key, _)) in fields.iter().enumerate() {
                    let key_idx = self.chunk.add_constant(NValue::string(key.as_str().into()));
                    self.emit(Op::LoadConst(key_idx), span);
                    self.gen_var(&temps[i], span)?;
                }
                let count = Self::checked_u16(fields.len(), "record fields", span)?;
                self.emit(Op::MakeRecord(count), span);
                self.end_scope(span);
            }
            Expr::MakeTuple(elems, _) => {
                for elem in elems {
                    self.gen_expr(elem, span)?;
                }
                let count = Self::checked_u16(elems.len(), "tuple elements", span)?;
                self.emit(Op::MakeTuple(count), span);
            }
            Expr::MakeRange(start, end) => {
                self.gen_expr(start, span)?;
                self.gen_expr(end, span)?;
                self.emit(Op::MakeRange, span);
            }
            Expr::UpdateRecord { base, updates, .. } => {
                // Pre-evaluate update values into temp locals (same fix as
                // MakeRecord — see BASEL-223).
                self.begin_scope();
                let temps: Vec<String> = (0..updates.len())
                    .map(|i| format!("__upd_{}", i))
                    .collect();
                for (i, (_, val)) in updates.iter().enumerate() {
                    self.gen_expr(val, span)?;
                    self.declare_local(&temps[i]);
                }
                self.gen_expr(base, span)?;
                for (i, (key, _)) in updates.iter().enumerate() {
                    let key_idx = self.chunk.add_constant(NValue::string(key.as_str().into()));
                    self.emit(Op::LoadConst(key_idx), span);
                    self.gen_var(&temps[i], span)?;
                }
                let count = Self::checked_u16(updates.len(), "record update fields", span)?;
                self.emit(Op::UpdateRecord(count), span);
                self.end_scope(span);
            }

            Expr::GetField { object, field, field_idx, .. } => {
                self.gen_expr(object, span)?;
                let name_idx = self
                    .chunk
                    .add_constant(NValue::string(field.as_str().into()));
                if let Some(idx) = field_idx {
                    self.emit(Op::GetFieldIdx(*idx, name_idx), span);
                } else {
                    self.emit(Op::GetField(name_idx), span);
                }
            }

            Expr::BinOp { op, lhs, rhs, ty } => {
                self.gen_expr(lhs, span)?;
                self.gen_expr(rhs, span)?;
                let both_int = matches!(ty, Some(Type::Int));
                let opcode = match op {
                    BinOp::Add if both_int => Op::AddInt,
                    BinOp::Add => Op::Add,
                    BinOp::Sub if both_int => Op::SubInt,
                    BinOp::Sub => Op::Sub,
                    BinOp::Mul if both_int => Op::MulInt,
                    BinOp::Mul => Op::Mul,
                    BinOp::Div if both_int => Op::DivInt,
                    BinOp::Div => Op::Div,
                    BinOp::Mod if both_int => Op::ModInt,
                    BinOp::Mod => Op::Mod,
                    BinOp::Eq => Op::Eq,
                    BinOp::Ne => Op::Ne,
                    BinOp::Lt if both_int => Op::LtInt,
                    BinOp::Lt => Op::Lt,
                    BinOp::Gt if both_int => Op::GtInt,
                    BinOp::Gt => Op::Gt,
                    BinOp::Le if both_int => Op::LeInt,
                    BinOp::Le => Op::Le,
                    BinOp::Ge if both_int => Op::GeInt,
                    BinOp::Ge => Op::Ge,
                    BinOp::ListConcat => Op::ListConcat,
                };
                self.emit(opcode, span);
            }

            Expr::UnaryOp { op, operand, .. } => {
                self.gen_expr(operand, span)?;
                match op {
                    UnaryOp::Neg => self.emit(Op::Negate, span),
                    UnaryOp::Not => self.emit(Op::Not, span),
                };
            }

            Expr::And(lhs, rhs) => {
                self.gen_expr(lhs, span)?;
                let jump_idx = self.emit(Op::JumpIfFalse(0), span);
                self.gen_expr(rhs, span)?;
                let end_idx = self.emit(Op::Jump(0), span);
                self.chunk.patch_jump(jump_idx);
                let false_const = self.chunk.add_constant(NValue::bool(false));
                self.emit(Op::LoadConst(false_const), span);
                self.chunk.patch_jump(end_idx);
            }
            Expr::Or(lhs, rhs) => {
                self.gen_expr(lhs, span)?;
                let jump_idx = self.emit(Op::JumpIfFalse(0), span);
                let true_const = self.chunk.add_constant(NValue::bool(true));
                self.emit(Op::LoadConst(true_const), span);
                let end_jump = self.emit(Op::Jump(0), span);
                self.chunk.patch_jump(jump_idx);
                self.gen_expr(rhs, span)?;
                self.chunk.patch_jump(end_jump);
            }

            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.gen_expr(condition, span)?;
                let then_jump = self.emit(Op::JumpIfFalse(0), span);
                self.gen_expr(then_branch, span)?;

                if let Some(else_br) = else_branch {
                    let else_jump = self.emit(Op::Jump(0), span);
                    self.chunk.patch_jump(then_jump);
                    self.gen_expr(else_br, span)?;
                    self.chunk.patch_jump(else_jump);
                } else {
                    let else_jump = self.emit(Op::Jump(0), span);
                    self.chunk.patch_jump(then_jump);
                    let idx = self.chunk.add_constant(NValue::unit());
                    self.emit(Op::LoadConst(idx), span);
                    self.chunk.patch_jump(else_jump);
                }
            }

            Expr::Match { subject, arms, .. } => {
                self.gen_match(subject, arms, span)?;
            }

            Expr::For {
                binding,
                iterable,
                body,
            } => {
                self.gen_for(binding, iterable, body, span)?;
            }

            Expr::Hole => {
                let msg = "Typed hole (??) encountered at runtime";
                let idx = self.chunk.add_constant(NValue::string(msg.into()));
                self.emit(Op::Halt(idx), span);
            }

            Expr::Let { pattern, value, .. } => {
                self.gen_expr(value, span)?;
                self.gen_let_pattern(pattern, span)?;
            }

            Expr::Block(exprs, _) => {
                self.gen_block(exprs, span)?;
            }

            Expr::Lambda { params, body, .. } => {
                self.gen_lambda(params, body, span)?;
            }

            // MakeClosure/GetClosureVar are JIT-only IR nodes produced by lambda lifting.
            // The bytecode codegen path uses Lambda directly (not lifted), so these
            // should never appear. If they do, it's a compiler bug.
            Expr::MakeClosure { .. } | Expr::GetClosureVar(_) => {
                return Err(CompileError {
                    message: "MakeClosure/GetClosureVar in bytecode codegen (should only appear in JIT path)".into(),
                    line: span.line,
                    col: span.col,
                });
            }

            Expr::Try { expr, .. } => {
                self.gen_try(expr, span)?;
            }

            Expr::Concat(parts) => {
                self.gen_concat(parts, span)?;
            }

            Expr::WithHandlers { handlers, body } => {
                self.gen_with_handlers(handlers, body, span)?;
            }

            Expr::HandleEffect { body, clauses } => {
                self.gen_handle_effect(body, clauses, span)?;
            }

            Expr::PerformEffect {
                effect,
                method,
                args,
                ..
            } => {
                self.gen_perform_effect(effect, method, args, span)?;
            }

            Expr::Expect { actual, matcher } => {
                self.gen_expect(actual, matcher, span)?;
            }
        }
        Ok(())
    }

    // -----------------------------------------------------------------------
    // Variables
    // -----------------------------------------------------------------------

    fn gen_var(&mut self, name: &str, span: &Span) -> Result<(), CompileError> {
        // Search locals
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.name == name {
                let slot = Self::checked_u16(i, "local variables", span)?;
                self.emit(Op::GetLocal(slot), span);
                return Ok(());
            }
        }
        // Search upvalues
        for (i, uv) in self.upvalues.iter().enumerate() {
            if uv.name == name {
                self.emit(Op::GetUpvalue(i as u8), span);
                return Ok(());
            }
        }
        // Try to capture from enclosing
        if let Some(uv_idx) = self.resolve_upvalue(name) {
            self.emit(Op::GetUpvalue(uv_idx as u8), span);
            return Ok(());
        }
        // Global function
        if let Some(&chunk_idx) = self.functions.get(name) {
            let idx = self.chunk.add_constant(NValue::function(chunk_idx));
            self.emit(Op::LoadConst(idx), span);
            return Ok(());
        }
        Err(CompileError {
            message: format!("Undefined variable: {}", name),
            line: span.line,
            col: span.col,
        })
    }

    fn resolve_upvalue(&mut self, name: &str) -> Option<usize> {
        if self.enclosing.is_empty() {
            return None;
        }
        let depth = self.enclosing.len();
        let mut found_at: Option<(usize, u16, bool)> = None;

        for d in (0..depth).rev() {
            for (slot, local) in self.enclosing[d].locals.iter().enumerate().rev() {
                if local.name == name {
                    found_at = Some((d, slot as u16, true));
                    break;
                }
            }
            if found_at.is_some() {
                break;
            }
            for (i, uv) in self.enclosing[d].upvalues.iter().enumerate() {
                if uv.name == name {
                    found_at = Some((d, i as u16, false));
                    break;
                }
            }
            if found_at.is_some() {
                break;
            }
        }

        let (found_depth, mut index, mut is_local) = found_at?;

        for d in (found_depth + 1)..depth {
            let uv_idx = self.enclosing[d].upvalues.len();
            self.enclosing[d].upvalues.push(Upvalue {
                name: name.to_string(),
                is_local,
                index,
            });
            index = uv_idx as u16;
            is_local = false;
        }

        let uv_idx = self.upvalues.len();
        self.upvalues.push(Upvalue {
            name: name.to_string(),
            is_local,
            index,
        });
        Some(uv_idx)
    }

    // -----------------------------------------------------------------------
    // Calls
    // -----------------------------------------------------------------------

    fn gen_call_direct(
        &mut self,
        name: &str,
        args: &[Expr],
        span: &Span,
    ) -> Result<(), CompileError> {
        if let Some(&chunk_idx) = self.functions.get(name) {
            // Emit args first, then CallDirect — no function value pushed
            for arg in args {
                self.gen_expr(arg, span)?;
            }
            let ci = Self::checked_u16(chunk_idx, "function chunks", span)?;
            let argc = Self::checked_u8(args.len(), "arguments", span)?;
            self.emit(Op::CallDirect(ci, argc), span);
            Ok(())
        } else {
            // Fall back to variable lookup (might be a local/upvalue holding a function)
            self.gen_var(name, span)?;
            for arg in args {
                self.gen_expr(arg, span)?;
            }
            let argc = Self::checked_u8(args.len(), "arguments", span)?;
            self.emit(Op::Call(argc), span);
            Ok(())
        }
    }

    fn gen_call_native(
        &mut self,
        module: &str,
        method: &str,
        args: &[Expr],
        span: &Span,
    ) -> Result<(), CompileError> {
        let qualified = if module.is_empty() {
            method.to_string()
        } else {
            format!("{}.{}", module, method)
        };
        if let Some(fn_id) = self.natives.lookup(&qualified) {
            for arg in args {
                self.gen_expr(arg, span)?;
            }
            let argc = Self::checked_u8(args.len(), "native arguments", span)?;
            self.emit(Op::CallNative(fn_id, argc), span);
            Ok(())
        } else {
            Err(CompileError {
                message: format!("Unknown native function: {}", qualified),
                line: span.line,
                col: span.col,
            })
        }
    }

    // -----------------------------------------------------------------------
    // Match
    // -----------------------------------------------------------------------

    fn gen_match(
        &mut self,
        subject: &Expr,
        arms: &[MatchArm],
        span: &Span,
    ) -> Result<(), CompileError> {
        self.gen_expr(subject, span)?;
        self.declare_local("__match_subject");
        let subject_slot = (self.locals.len() - 1) as u16;

        let mut end_jumps = Vec::new();

        for arm in arms {
            match &arm.pattern {
                Pattern::Wildcard => {
                    self.gen_expr(&arm.body, span)?;
                    end_jumps.push(self.emit(Op::Jump(0), span));
                }
                Pattern::Var(name) => {
                    self.emit(Op::GetLocal(subject_slot), span);
                    self.begin_scope();
                    self.declare_local(name);
                    self.gen_expr(&arm.body, span)?;
                    self.end_scope(span);
                    end_jumps.push(self.emit(Op::Jump(0), span));
                }
                Pattern::Literal(lit_expr) => {
                    self.emit(Op::GetLocal(subject_slot), span);
                    self.gen_expr(lit_expr, span)?;
                    self.emit(Op::Eq, span);
                    let skip_jump = self.emit(Op::JumpIfFalse(0), span);

                    self.gen_expr(&arm.body, span)?;
                    end_jumps.push(self.emit(Op::Jump(0), span));

                    self.chunk.patch_jump(skip_jump);
                }
                Pattern::Constructor(tag, sub_patterns) => {
                    // Check tag matches
                    self.emit(Op::GetLocal(subject_slot), span);
                    self.emit(Op::EnumTag, span);
                    let tag_const = self.chunk.add_constant(NValue::string(tag.as_str().into()));
                    self.emit(Op::LoadConst(tag_const), span);
                    self.emit(Op::Eq, span);
                    let skip_jump = self.emit(Op::JumpIfFalse(0), span);

                    self.begin_scope();
                    if sub_patterns.len() == 1 {
                        // Single binding: Some(v)
                        self.emit(Op::GetLocal(subject_slot), span);
                        self.emit(Op::EnumPayload, span);
                        match &sub_patterns[0] {
                            Pattern::Var(name) => self.declare_local(name),
                            Pattern::Wildcard => self.declare_local("_"),
                            _ => {}
                        }
                    } else if sub_patterns.len() > 1 {
                        // Multi-binding: Rectangle(w, h)
                        self.emit(Op::GetLocal(subject_slot), span);
                        self.emit(Op::EnumPayload, span);
                        self.declare_local("__payload");
                        let payload_slot = (self.locals.len() - 1) as u16;

                        for (i, pat) in sub_patterns.iter().enumerate() {
                            self.emit(Op::GetLocal(payload_slot), span);
                            self.emit(Op::TupleGet(i as u16), span);
                            match pat {
                                Pattern::Var(name) => self.declare_local(name),
                                Pattern::Wildcard => self.declare_local("_"),
                                _ => {}
                            }
                        }
                    }

                    self.gen_expr(&arm.body, span)?;
                    self.end_scope(span);
                    end_jumps.push(self.emit(Op::Jump(0), span));

                    self.chunk.patch_jump(skip_jump);
                }
                Pattern::Tuple(sub_patterns) => {
                    self.begin_scope();
                    for (i, pat) in sub_patterns.iter().enumerate() {
                        self.emit(Op::GetLocal(subject_slot), span);
                        self.emit(Op::TupleGet(i as u16), span);
                        match pat {
                            Pattern::Var(name) => self.declare_local(name),
                            Pattern::Wildcard => self.declare_local("_"),
                            _ => {}
                        }
                    }
                    self.gen_expr(&arm.body, span)?;
                    self.end_scope(span);
                    end_jumps.push(self.emit(Op::Jump(0), span));
                }
                Pattern::Record(fields) => {
                    // Record patterns always match structurally
                    self.begin_scope();
                    for (field_name, sub_pattern) in fields {
                        self.emit(Op::GetLocal(subject_slot), span);
                        let name_idx = self
                            .chunk
                            .add_constant(NValue::string(field_name.as_str().into()));
                        self.emit(Op::GetField(name_idx), span);
                        match sub_pattern {
                            Pattern::Var(name) => self.declare_local(name),
                            Pattern::Wildcard => self.declare_local("_"),
                            _ => {}
                        }
                    }
                    self.gen_expr(&arm.body, span)?;
                    self.end_scope(span);
                    end_jumps.push(self.emit(Op::Jump(0), span));
                }
            }
        }

        // Default: push Unit
        let idx = self.chunk.add_constant(NValue::unit());
        self.emit(Op::LoadConst(idx), span);

        for jump in &end_jumps {
            self.chunk.patch_jump(*jump);
        }

        // Clean up subject
        self.locals.pop();
        self.emit(Op::CloseScope(1), span);

        Ok(())
    }

    // -----------------------------------------------------------------------
    // For loop
    // -----------------------------------------------------------------------

    fn gen_for(
        &mut self,
        binding: &str,
        iterable: &Expr,
        body: &Expr,
        span: &Span,
    ) -> Result<(), CompileError> {
        self.gen_expr(iterable, span)?;
        self.declare_local("__for_list");
        let list_slot = (self.locals.len() - 1) as u16;

        let zero = self.chunk.add_constant(NValue::int(0));
        self.emit(Op::LoadConst(zero), span);
        self.declare_local("__for_idx");
        let idx_slot = (self.locals.len() - 1) as u16;

        let loop_start = self.chunk.code.len();

        self.emit(Op::GetLocal(list_slot), span);
        self.emit(Op::ListLen, span);
        self.emit(Op::GetLocal(idx_slot), span);
        self.emit(Op::Lt, span);
        let exit_jump = self.emit(Op::JumpIfFalse(0), span);

        self.emit(Op::GetLocal(list_slot), span);
        self.emit(Op::GetLocal(idx_slot), span);
        self.emit(Op::ListGet, span);

        self.begin_scope();
        self.declare_local(binding);

        self.gen_expr(body, span)?;
        self.emit(Op::Pop, span);

        self.end_scope(span);

        // Increment index
        self.emit(Op::GetLocal(idx_slot), span);
        let one = self.chunk.add_constant(NValue::int(1));
        self.emit(Op::LoadConst(one), span);
        self.emit(Op::Add, span);
        self.emit(Op::SetLocal(idx_slot), span);
        self.emit(Op::Pop, span);

        let jump_back_dist =
            Self::checked_u16(self.chunk.code.len() - loop_start + 1, "jump distance", span)?;
        self.emit(Op::JumpBack(jump_back_dist), span);

        self.chunk.patch_jump(exit_jump);

        // Cleanup
        self.locals.pop(); // __for_idx
        self.locals.pop(); // __for_list
        self.emit(Op::PopN(2), span);

        let unit = self.chunk.add_constant(NValue::unit());
        self.emit(Op::LoadConst(unit), span);

        Ok(())
    }

    // -----------------------------------------------------------------------
    // Let binding
    // -----------------------------------------------------------------------

    fn gen_let_pattern(&mut self, pattern: &Pattern, span: &Span) -> Result<(), CompileError> {
        match pattern {
            Pattern::Var(name) => {
                self.declare_local(name);
            }
            Pattern::Tuple(pats) => {
                self.declare_local("__let_tuple");
                let tuple_slot = (self.locals.len() - 1) as u16;
                for (i, pat) in pats.iter().enumerate() {
                    self.emit(Op::GetLocal(tuple_slot), span);
                    self.emit(Op::TupleGet(i as u16), span);
                    match pat {
                        Pattern::Var(name) => self.declare_local(name),
                        Pattern::Wildcard => self.declare_local("_"),
                        _ => {}
                    }
                }
            }
            Pattern::Record(fields) => {
                self.declare_local("__let_record");
                let record_slot = (self.locals.len() - 1) as u16;
                for (field_name, sub_pattern) in fields {
                    self.emit(Op::GetLocal(record_slot), span);
                    let name_idx = self
                        .chunk
                        .add_constant(NValue::string(field_name.as_str().into()));
                    self.emit(Op::GetField(name_idx), span);
                    match sub_pattern {
                        Pattern::Var(name) => self.declare_local(name),
                        Pattern::Wildcard => self.declare_local("_"),
                        _ => {}
                    }
                }
            }
            Pattern::Wildcard => {
                self.declare_local("_");
            }
            _ => {}
        }
        Ok(())
    }

    // -----------------------------------------------------------------------
    // Block
    // -----------------------------------------------------------------------

    fn gen_block(&mut self, exprs: &[Expr], span: &Span) -> Result<(), CompileError> {
        if exprs.is_empty() {
            let idx = self.chunk.add_constant(NValue::unit());
            self.emit(Op::LoadConst(idx), span);
            return Ok(());
        }

        self.begin_scope();
        let mut last_was_expr = false;

        for expr in exprs.iter() {
            if last_was_expr {
                self.emit(Op::Pop, span);
            }
            let is_let = matches!(expr, Expr::Let { .. });
            self.gen_expr(expr, span)?;
            last_was_expr = !is_let;
        }

        if !last_was_expr {
            let idx = self.chunk.add_constant(NValue::unit());
            self.emit(Op::LoadConst(idx), span);
        }

        self.end_scope(span);
        Ok(())
    }

    // -----------------------------------------------------------------------
    // Lambda
    // -----------------------------------------------------------------------

    fn gen_lambda(
        &mut self,
        params: &[String],
        body: &Expr,
        span: &Span,
    ) -> Result<(), CompileError> {
        self.enter_function();

        for param in params {
            self.declare_local(param);
        }

        self.gen_expr(body, span)?;

        let (func_chunk, captured) = self.leave_function();
        let chunk_idx = self.compiled_chunks.len();
        self.compiled_chunks.push(func_chunk);

        if captured.is_empty() {
            let const_idx = self.chunk.add_constant(NValue::function(chunk_idx));
            self.emit(Op::LoadConst(const_idx), span);
        } else {
            for uv in &captured {
                if uv.is_local {
                    self.emit(Op::GetLocal(uv.index), span);
                } else {
                    self.emit(Op::GetUpvalue(uv.index as u8), span);
                }
            }
            let ci = Self::checked_u16(chunk_idx, "function chunks", span)?;
            let cap_count = Self::checked_u8(captured.len(), "captured upvalues", span)?;
            self.emit(Op::MakeClosure(ci, cap_count), span);
        }

        Ok(())
    }

    // -----------------------------------------------------------------------
    // Try expression
    // -----------------------------------------------------------------------

    fn gen_try(&mut self, expr: &Expr, span: &Span) -> Result<(), CompileError> {
        self.gen_expr(expr, span)?;

        self.declare_local("__try_val");
        let val_slot = (self.locals.len() - 1) as u16;

        // Check Err
        self.emit(Op::GetLocal(val_slot), span);
        self.emit(Op::EnumTag, span);
        let err_tag = self.chunk.add_constant(NValue::string("Err".into()));
        self.emit(Op::LoadConst(err_tag), span);
        self.emit(Op::Eq, span);
        let not_err = self.emit(Op::JumpIfFalse(0), span);
        self.emit(Op::GetLocal(val_slot), span);
        self.emit(Op::Return, span);
        self.chunk.patch_jump(not_err);

        // Check None
        self.emit(Op::GetLocal(val_slot), span);
        self.emit(Op::EnumTag, span);
        let none_tag = self.chunk.add_constant(NValue::string("None".into()));
        self.emit(Op::LoadConst(none_tag), span);
        self.emit(Op::Eq, span);
        let not_none = self.emit(Op::JumpIfFalse(0), span);
        self.emit(Op::GetLocal(val_slot), span);
        self.emit(Op::Return, span);
        self.chunk.patch_jump(not_none);

        // Extract payload
        self.emit(Op::GetLocal(val_slot), span);
        self.emit(Op::EnumPayload, span);

        self.locals.pop();
        self.emit(Op::CloseScope(1), span);

        Ok(())
    }

    // -----------------------------------------------------------------------
    // String concatenation
    // -----------------------------------------------------------------------

    fn gen_concat(&mut self, parts: &[Expr], span: &Span) -> Result<(), CompileError> {
        if parts.is_empty() {
            let idx = self.chunk.add_constant(NValue::string("".into()));
            self.emit(Op::LoadConst(idx), span);
            return Ok(());
        }

        let has_interpolation = parts.iter().any(|p| !matches!(p, Expr::String(_)));

        // When mixing string literals and expressions (e.g. "prefix ${expr}"),
        // expression parts may contain inlined Block/Let from the optimizer.
        // Evaluating those while string literals sit as untracked values on the
        // stack corrupts local-slot numbering. Fix: pre-evaluate all non-string
        // parts into temp locals so the stack is clean during each evaluation.
        let needs_temp_locals = has_interpolation
            && parts.len() > 1
            && parts.iter().any(|p| matches!(p, Expr::String(_)));

        if needs_temp_locals {
            self.begin_scope();

            // Phase 1: evaluate non-string parts into temp locals
            let mut prepared: Vec<Option<String>> = Vec::with_capacity(parts.len());
            for (i, part) in parts.iter().enumerate() {
                if matches!(part, Expr::String(_)) {
                    prepared.push(None); // will emit directly in phase 2
                } else {
                    self.gen_expr(part, span)?;
                    let temp = format!("__concat_{}", i);
                    self.declare_local(&temp);
                    prepared.push(Some(temp));
                }
            }

            // Phase 2: load all parts and concatenate
            for (segment_count, (i, prep)) in prepared.iter().enumerate().enumerate() {
                match prep {
                    None => self.gen_expr(&parts[i], span)?,
                    Some(name) => self.gen_var(name, span)?,
                }
                if segment_count > 0 {
                    self.emit(Op::Concat, span);
                }
            }

            self.end_scope(span);
        } else {
            // Simple path: no interleaving risk (all expressions or single part)
            let mut segment_count = 0;
            for part in parts {
                self.gen_expr(part, span)?;
                if segment_count > 0 {
                    self.emit(Op::Concat, span);
                }
                segment_count += 1;
            }

            // If single interpolation expression, ensure it's converted to string via Concat
            if segment_count == 1 && has_interpolation {
                let idx = self.chunk.add_constant(NValue::string("".into()));
                self.emit(Op::LoadConst(idx), span);
                self.emit(Op::Concat, span);
            }
        }

        Ok(())
    }

    // -----------------------------------------------------------------------
    // Effect handlers
    // -----------------------------------------------------------------------

    fn gen_with_handlers(
        &mut self,
        handlers: &[(String, Vec<(String, Expr)>)],
        body: &Expr,
        span: &Span,
    ) -> Result<(), CompileError> {
        // Build outer record: { EffectName: { method: fn, ... }, ... }
        for (effect_name, methods) in handlers {
            let eff_key = self.chunk.add_constant(NValue::string(effect_name.as_str().into()));
            self.emit(Op::LoadConst(eff_key), span);

            if methods.len() == 1 && methods[0].0 == "__record__" {
                // with { Effect: expr } form — handler value is already a record
                self.gen_expr(&methods[0].1, span)?;
            } else {
                // handle form — build inner method record
                for (method_key, handler_fn) in methods {
                    let mk = self.chunk.add_constant(NValue::string(method_key.as_str().into()));
                    self.emit(Op::LoadConst(mk), span);
                    self.gen_expr(handler_fn, span)?;
                }
                self.emit(Op::MakeRecord(methods.len() as u16), span);
            }
        }
        self.emit(Op::MakeRecord(handlers.len() as u16), span);
        self.emit(Op::PushHandler, span);

        self.gen_expr(body, span)?;

        self.emit(Op::PopHandler, span);
        Ok(())
    }

    fn gen_handle_effect(
        &mut self,
        body: &Expr,
        clauses: &[crate::vm::ir::HandlerClause],
        span: &Span,
    ) -> Result<(), CompileError> {
        // Build handler record same as gen_with_handlers:
        // { EffectName: { method: handler_fn, ... }, ... }
        // For non-tail-resumptive handlers, the lambda gets an extra `resume` param.
        // Group clauses by effect name.
        let mut grouped: Vec<(String, Vec<(&str, &crate::vm::ir::HandlerClause)>)> = Vec::new();
        let mut effect_order: Vec<String> = Vec::new();

        for clause in clauses {
            if !effect_order.contains(&clause.effect) {
                effect_order.push(clause.effect.clone());
                grouped.push((clause.effect.clone(), vec![(&clause.method, clause)]));
            } else {
                for (eff, methods) in &mut grouped {
                    if *eff == clause.effect {
                        methods.push((&clause.method, clause));
                        break;
                    }
                }
            }
        }

        let all_tail_resumptive = clauses.iter().all(|c| c.is_tail_resumptive);
        for (effect_name, methods) in &grouped {
            let eff_key = self
                .chunk
                .add_constant(NValue::string(effect_name.as_str().into()));
            self.emit(Op::LoadConst(eff_key), span);

            for (method_key, clause) in methods {
                let mk = self
                    .chunk
                    .add_constant(NValue::string((*method_key).into()));
                self.emit(Op::LoadConst(mk), span);

                // Compile handler body as a lambda.
                // The lambda parameters depend on whether we use PushHandler
                // (all tail-resumptive) or PushResumableHandler (mixed).
                let mut lambda_params: Vec<String> = clause.params.clone();
                if all_tail_resumptive {
                    // PushHandler path: no continuation arg passed by VM.
                    // Strip resume(expr) wrapper — just compile the inner expr.
                    let body = Self::strip_tail_resume(&clause.body);
                    self.gen_lambda(&lambda_params, body, span)?;
                } else {
                    // PushResumableHandler path: VM always passes resume
                    // continuation as the last arg to ALL handlers.
                    lambda_params.push("resume".to_string());
                    if clause.is_tail_resumptive {
                        // Tail-resumptive in mixed block: accept resume param
                        // but strip the resume() call, just return the inner expr.
                        let body = Self::strip_tail_resume(&clause.body);
                        self.gen_lambda(&lambda_params, body, span)?;
                    } else {
                        self.gen_lambda(&lambda_params, &clause.body, span)?;
                    }
                }
            }
            self.emit(Op::MakeRecord(methods.len() as u16), span);
        }
        self.emit(Op::MakeRecord(grouped.len() as u16), span);


        eprintln!("[DEBUG codegen] all_tail_resumptive={}, clauses={}", all_tail_resumptive, clauses.len());
        if all_tail_resumptive {
            // All handlers are tail-resumptive: no continuation capture needed.
            // Use PushHandler/PopHandler (like `with` form).
            self.emit(Op::PushHandler, span);
            self.gen_expr(body, span)?;
            self.emit(Op::PopHandler, span);
        } else {
            // At least one handler needs resume: use PushResumableHandler.
            // The VM will pass a resume continuation to ALL handlers uniformly,
            // so even tail-resumptive ones need a `resume` param to accept it
            // (they just won't use it — the caller discards it on return).
            let handler_idx = self.emit(Op::PushResumableHandler(0), span);
            self.gen_expr(body, span)?;
            self.emit(Op::PopHandler, span);
            // Patch the skip offset for abort handlers
            let after_pop = self.chunk.code.len();
            let skip = (after_pop - handler_idx - 1) as u16;
            self.chunk.code[handler_idx] = Op::PushResumableHandler(skip);
        }
        Ok(())
    }

    /// Strip the `resume(expr)` wrapper from a tail-resumptive handler body.
    /// Returns the inner expression (the argument to resume).
    fn strip_tail_resume(expr: &Expr) -> &Expr {
        match expr {
            Expr::CallIndirect { args, .. } if args.len() == 1 => &args[0],
            Expr::Block(stmts, _) if stmts.len() == 1 => Self::strip_tail_resume(&stmts[0]),
            _ => expr,
        }
    }

    fn gen_perform_effect(
        &mut self,
        effect: &str,
        method: &str,
        args: &[Expr],
        span: &Span,
    ) -> Result<(), CompileError> {
        // Push args onto stack
        for arg in args {
            self.gen_expr(arg, span)?;
        }
        // Emit perform with constant pool key "Effect.method"
        let key = format!("{}.{}", effect, method);
        let name_idx = self.chunk.add_constant(NValue::string(key.into()));
        let arg_count = Self::checked_u8(args.len(), "perform effect args", span)?;
        self.emit(Op::PerformEffect(name_idx, arg_count), span);
        Ok(())
    }

    // -----------------------------------------------------------------------
    // Expect (test matchers)
    // -----------------------------------------------------------------------

    fn gen_expect(
        &mut self,
        actual: &Expr,
        matcher: &Matcher,
        span: &Span,
    ) -> Result<(), CompileError> {
        match matcher {
            Matcher::Equal(expected) => {
                self.gen_expr(actual, span)?;
                self.gen_expr(expected, span)?;
                self.emit(Op::Eq, span);
            }
            Matcher::BeOk => {
                // Check if result is Ok variant
                self.gen_expr(actual, span)?;
                self.emit(Op::EnumTag, span);
                let tag = self.chunk.add_constant(NValue::string("Ok".into()));
                self.emit(Op::LoadConst(tag), span);
                self.emit(Op::Eq, span);
            }
            Matcher::BeSome => {
                self.gen_expr(actual, span)?;
                self.emit(Op::EnumTag, span);
                let tag = self.chunk.add_constant(NValue::string("Some".into()));
                self.emit(Op::LoadConst(tag), span);
                self.emit(Op::Eq, span);
            }
            Matcher::BeNone => {
                self.gen_expr(actual, span)?;
                self.emit(Op::EnumTag, span);
                let tag = self.chunk.add_constant(NValue::string("None".into()));
                self.emit(Op::LoadConst(tag), span);
                self.emit(Op::Eq, span);
            }
            Matcher::BeEmpty => {
                self.gen_expr(actual, span)?;
                self.emit(Op::ListLen, span);
                let zero = self.chunk.add_constant(NValue::int(0));
                self.emit(Op::LoadConst(zero), span);
                self.emit(Op::Eq, span);
            }
            Matcher::HaveLength(expected) => {
                self.gen_expr(actual, span)?;
                self.emit(Op::ListLen, span);
                self.gen_expr(expected, span)?;
                self.emit(Op::Eq, span);
            }
            Matcher::Contain(expected) => {
                // Use native __test_contains if available, else fall back
                let qualified = "__test_contains";
                if let Some(fn_id) = self.natives.lookup(qualified) {
                    self.gen_expr(actual, span)?;
                    self.gen_expr(expected, span)?;
                    self.emit(Op::CallNative(fn_id, 2), span);
                } else {
                    // Fall back to simple equality check
                    self.gen_expr(actual, span)?;
                    self.gen_expr(expected, span)?;
                    self.emit(Op::Eq, span);
                }
            }
            Matcher::StartWith(expected) => {
                let qualified = "String.starts_with";
                if let Some(fn_id) = self.natives.lookup(qualified) {
                    self.gen_expr(actual, span)?;
                    self.gen_expr(expected, span)?;
                    self.emit(Op::CallNative(fn_id, 2), span);
                } else {
                    // Fall back
                    self.gen_expr(actual, span)?;
                    self.gen_expr(expected, span)?;
                    self.emit(Op::Eq, span);
                }
            }
            Matcher::Satisfy(pred) => {
                // Apply the predicate to the actual value
                self.gen_expr(pred, span)?;
                self.gen_expr(actual, span)?;
                self.emit(Op::Call(1), span);
            }
            Matcher::Be(_pattern) => {
                // TODO: match pattern against actual; for now always true
                let t = self.chunk.add_constant(NValue::bool(true));
                self.emit(Op::LoadConst(t), span);
            }
        }
        Ok(())
    }

    // -----------------------------------------------------------------------
    // Test program generation
    // -----------------------------------------------------------------------

    /// Generate a TestProgram from an IrTestModule.
    pub fn generate_test_program(
        mut self,
        module: &IrTestModule,
    ) -> Result<super::chunk::TestProgram, CompileError> {
        let chunk_offset = self.compiled_chunks.len();

        // Register all function names
        for (i, func) in module.functions.iter().enumerate() {
            self.functions.insert(func.name.clone(), chunk_offset + i);
        }

        // Pre-allocate function chunk slots
        for _ in 0..module.functions.len() {
            self.compiled_chunks.push(Chunk::new());
        }

        // Compile function bodies
        for (i, func) in module.functions.iter().enumerate() {
            self.chunk = Chunk::new();
            self.locals.clear();
            self.scope_depth = 0;

            for param in &func.params {
                self.declare_local(param);
            }

            self.gen_expr(&func.body, &func.span)?;
            let chunk = self.finish_chunk();
            self.compiled_chunks[chunk_offset + i] = chunk;
        }

        // Compile each test body into its own chunk.
        // NOTE: We cannot pre-calculate chunk indices because gen_expr may
        // push additional chunks (e.g. for lambdas). We record the actual
        // index after pushing each test chunk.
        let mut compiled_tests = Vec::new();

        for test in module.tests.iter() {
            self.chunk = Chunk::new();
            self.locals.clear();
            self.scope_depth = 0;

            let default_span = Span {
                line: test.line,
                col: test.col,
                start_byte: 0,
                end_byte: 0,
            };
            self.gen_expr(&test.body, &default_span)?;
            let chunk = self.finish_chunk();
            let chunk_idx = self.compiled_chunks.len();
            self.compiled_chunks.push(chunk);

            compiled_tests.push(super::chunk::CompiledTest {
                name: test.name.clone(),
                chunk_idx,
                line: test.line,
                col: test.col,
                end_line: test.end_line,
                end_col: test.end_col,
                skip: test.skip,
            });
        }

        Ok(super::chunk::TestProgram {
            program: Program {
                chunks: self.compiled_chunks,
                entry: chunk_offset + module.entry,
            },
            tests: compiled_tests,
        })
    }

    // -----------------------------------------------------------------------
    // Scope management
    // -----------------------------------------------------------------------

    fn declare_local(&mut self, name: &str) {
        self.locals.push(Local {
            name: name.to_string(),
            depth: self.scope_depth,
        });
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self, span: &Span) {
        self.scope_depth -= 1;
        let mut pop_count = 0u16;
        while let Some(local) = self.locals.last() {
            if local.depth <= self.scope_depth {
                break;
            }
            self.locals.pop();
            pop_count += 1;
        }
        if pop_count > 0 {
            self.chunk
                .emit(Op::CloseScope(pop_count), span.line, span.col);
        }
    }

    // -----------------------------------------------------------------------
    // Nested function helpers
    // -----------------------------------------------------------------------

    fn enter_function(&mut self) {
        self.enclosing.push(CodegenFrame {
            chunk: std::mem::replace(&mut self.chunk, Chunk::new()),
            locals: std::mem::take(&mut self.locals),
            scope_depth: self.scope_depth,
            upvalues: std::mem::take(&mut self.upvalues),
        });
        self.scope_depth = 0;
    }

    fn leave_function(&mut self) -> (Chunk, Vec<Upvalue>) {
        let func_chunk = self.finish_chunk();
        let func_upvalues = std::mem::take(&mut self.upvalues);
        let frame = self.enclosing.pop().unwrap();
        self.chunk = frame.chunk;
        self.locals = frame.locals;
        self.scope_depth = frame.scope_depth;
        self.upvalues = frame.upvalues;
        (func_chunk, func_upvalues)
    }

    fn finish_chunk(&mut self) -> Chunk {
        let mut chunk = std::mem::replace(&mut self.chunk, Chunk::new());
        chunk.ensure_return();
        chunk
    }

    // -----------------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------------

    fn emit(&mut self, op: Op, span: &Span) -> usize {
        self.chunk.emit(op, span.line, span.col)
    }

    fn checked_u16(val: usize, context: &str, span: &Span) -> Result<u16, CompileError> {
        u16::try_from(val).map_err(|_| CompileError {
            message: format!("Too many {} (max 65535, got {})", context, val),
            line: span.line,
            col: span.col,
        })
    }

    fn checked_u8(val: usize, context: &str, span: &Span) -> Result<u8, CompileError> {
        u8::try_from(val).map_err(|_| CompileError {
            message: format!("Too many {} (max 255, got {})", context, val),
            line: span.line,
            col: span.col,
        })
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::lower::Lowerer;
    use crate::vm::value::Value;
    use crate::vm::exec::Vm;

    fn eval_via_ir(source: &str) -> Value {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_baseline::LANGUAGE.into())
            .expect("Error loading grammar");
        let tree = parser.parse(source, None).expect("Parse failed");
        let root = tree.root_node();

        let natives = NativeRegistry::new();
        let mut lowerer = Lowerer::new(source, &natives, None);
        let module = lowerer
            .lower_module(&root)
            .unwrap_or_else(|e| panic!("Lower error: {}", e));

        let codegen = Codegen::new(&natives);
        let mut program = codegen
            .generate_program(&module)
            .unwrap_or_else(|e| panic!("Codegen error: {}", e));
        program.optimize();

        let mut vm = Vm::new();
        vm.execute_program(&program).expect("VM error")
    }

    #[test]
    fn ir_pipeline_integer() {
        let source = "fn main() -> Int = 42";
        assert_eq!(eval_via_ir(source), Value::Int(42));
    }

    #[test]
    fn ir_pipeline_arithmetic() {
        let source = "fn main() -> Int = 1 + 2 * 3";
        assert_eq!(eval_via_ir(source), Value::Int(7));
    }

    #[test]
    fn ir_pipeline_const_fold() {
        let source = "fn main() -> Int = (2 + 3) * (4 - 1)";
        assert_eq!(eval_via_ir(source), Value::Int(15));
    }

    #[test]
    fn ir_pipeline_function_call() {
        let source = "fn double(x: Int) -> Int = x * 2\nfn main() -> Int = double(5)";
        assert_eq!(eval_via_ir(source), Value::Int(10));
    }

    #[test]
    fn ir_pipeline_if_else() {
        let source = "fn main() -> Int = if true then 1 else 2";
        assert_eq!(eval_via_ir(source), Value::Int(1));
    }

    #[test]
    fn ir_pipeline_match() {
        let source = "fn main() -> Int = match 2\n  1 -> 10\n  2 -> 20\n  _ -> 99";
        assert_eq!(eval_via_ir(source), Value::Int(20));
    }

    #[test]
    fn ir_pipeline_let_binding() {
        let source = "fn main() -> Int = {\n  let x = 10\n  x + 1\n}";
        assert_eq!(eval_via_ir(source), Value::Int(11));
    }

    #[test]
    fn ir_pipeline_lambda() {
        let source = "fn main() -> Int = {\n  let f = |x| x + 1\n  f(3)\n}";
        assert_eq!(eval_via_ir(source), Value::Int(4));
    }

    #[test]
    fn ir_pipeline_closure() {
        let source = "fn main() -> Int = {\n  let x = 10\n  let f = |y| x + y\n  f(5)\n}";
        assert_eq!(eval_via_ir(source), Value::Int(15));
    }

    #[test]
    fn ir_pipeline_pipe() {
        let source = "fn double(x: Int) -> Int = x * 2\nfn main() -> Int = 5 |> double";
        assert_eq!(eval_via_ir(source), Value::Int(10));
    }

    #[test]
    fn ir_pipeline_enum_constructor() {
        let source = "fn main() -> Unknown = Some(42)";
        assert_eq!(
            eval_via_ir(source),
            Value::Enum("Some".into(), std::sync::Arc::new(Value::Int(42))),
        );
    }

    #[test]
    fn ir_pipeline_match_enum() {
        let source = "fn main() -> Int = match Some(42)\n  Some(v) -> v + 1\n  None -> 0";
        assert_eq!(eval_via_ir(source), Value::Int(43));
    }

    #[test]
    fn ir_pipeline_string_interpolation() {
        let source = "fn main() -> String = \"count: ${42}\"";
        assert_eq!(eval_via_ir(source), Value::String("count: 42".into()));
    }

    #[test]
    fn ir_pipeline_recursive() {
        let source = "\
fn factorial(n: Int) -> Int = if n <= 1 then 1 else n * factorial(n - 1)
fn main() -> Int = factorial(5)";
        assert_eq!(eval_via_ir(source), Value::Int(120));
    }

    #[test]
    fn ir_pipeline_tco() {
        let source = "\
fn countdown(n: Int) -> Int = if n <= 0 then 0 else countdown(n - 1)
fn main() -> Int = countdown(50000)";
        assert_eq!(eval_via_ir(source), Value::Int(0));
    }

    #[test]
    fn ir_pipeline_list() {
        let source = "fn main() -> Unknown = [1, 2, 3]";
        assert_eq!(
            eval_via_ir(source),
            Value::List(std::sync::Arc::new(vec![
                Value::Int(1),
                Value::Int(2),
                Value::Int(3)
            ])),
        );
    }

    #[test]
    fn ir_pipeline_record() {
        let source = "fn main() -> Unknown = { x: 1, y: 2 }";
        assert_eq!(
            eval_via_ir(source),
            Value::Record(std::sync::Arc::new(vec![
                ("x".into(), Value::Int(1)),
                ("y".into(), Value::Int(2)),
            ])),
        );
    }

    #[test]
    fn ir_pipeline_native_call() {
        let source = "fn main() -> Int = String.length(\"hello\")";
        assert_eq!(eval_via_ir(source), Value::Int(5));
    }

    #[test]
    fn ir_pipeline_pipe_native() {
        let source = "fn main() -> Int = \"hello\" |> String.length";
        assert_eq!(eval_via_ir(source), Value::Int(5));
    }

    #[test]
    fn ir_pipeline_try_ok() {
        let source = "fn main() -> Unknown = {\n  let x = Ok(42)\n  let v = x?\n  v + 1\n}";
        assert_eq!(eval_via_ir(source), Value::Int(43));
    }

    #[test]
    fn ir_pipeline_range() {
        let source = "fn main() -> Unknown = 1..4";
        assert_eq!(
            eval_via_ir(source),
            Value::List(std::sync::Arc::new(vec![
                Value::Int(1),
                Value::Int(2),
                Value::Int(3)
            ])),
        );
    }

    #[test]
    fn ir_pipeline_record_update() {
        let source = "fn main() -> Unknown = {\n  let r = { x: 1, y: 2 }\n  { ..r, x: 10 }\n}";
        assert_eq!(
            eval_via_ir(source),
            Value::Record(std::sync::Arc::new(vec![
                ("x".into(), Value::Int(10)),
                ("y".into(), Value::Int(2)),
            ])),
        );
    }

    #[test]
    fn ir_pipeline_bool_and() {
        let source = "fn main() -> Boolean = true && false";
        assert_eq!(eval_via_ir(source), Value::Bool(false));
    }

    #[test]
    fn ir_pipeline_bool_or() {
        let source = "fn main() -> Boolean = false || true";
        assert_eq!(eval_via_ir(source), Value::Bool(true));
    }

    #[test]
    fn ir_pipeline_for_loop() {
        let source = "fn main() -> Unknown = for x in 1..4 do x + 1";
        assert_eq!(eval_via_ir(source), Value::Unit);
    }

    #[test]
    fn ir_pipeline_tuple() {
        let source = "fn main() -> Unknown = (1, 2, 3)";
        assert_eq!(
            eval_via_ir(source),
            Value::Tuple(std::sync::Arc::new(vec![
                Value::Int(1),
                Value::Int(2),
                Value::Int(3)
            ])),
        );
    }

    #[test]
    fn ir_pipeline_unit() {
        let source = "fn main() -> Unknown = ()";
        assert_eq!(eval_via_ir(source), Value::Unit);
    }

    #[test]
    fn ir_pipeline_nested_closures() {
        let source = "\
fn make_adder(n: Int) -> (Int -> Int) = |x| n + x
fn main() -> Int = {
  let add5 = make_adder(5)
  add5(3)
}";
        assert_eq!(eval_via_ir(source), Value::Int(8));
    }

    #[test]
    fn ir_pipeline_hof_list_map() {
        let source = "fn main() -> Unknown = List.map([1, 2, 3], |x| x * 2)";
        assert_eq!(
            eval_via_ir(source),
            Value::List(std::sync::Arc::new(vec![
                Value::Int(2),
                Value::Int(4),
                Value::Int(6)
            ])),
        );
    }

    #[test]
    fn ir_pipeline_pipe_with_args() {
        let source = "fn add(a: Int, b: Int) -> Int = a + b\nfn main() -> Int = 3 |> add(4)";
        assert_eq!(eval_via_ir(source), Value::Int(7));
    }
}
