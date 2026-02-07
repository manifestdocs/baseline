use std::collections::HashMap;

use crate::analysis::types::Type;

use super::chunk::{Chunk, Op, Program};
use super::ir::*;
use super::natives::NativeRegistry;
use super::value::Value;

// ---------------------------------------------------------------------------
// Codegen Error
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct CodegenError {
    pub message: String,
    pub line: usize,
    pub col: usize,
}

impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}:{}] {}", self.line, self.col, self.message)
    }
}

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
    pub fn generate_program(mut self, module: &IrModule) -> Result<Program, CodegenError> {
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
    pub fn generate_module(mut self, functions: &[IrFunction]) -> Result<(Vec<Chunk>, HashMap<String, usize>), CodegenError> {
        // First pass: register names
        for (i, func) in functions.iter().enumerate() {
            self.functions.insert(func.name.clone(), i);
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
            self.compiled_chunks[i] = chunk;
        }

        Ok((self.compiled_chunks, self.functions))
    }

    // -----------------------------------------------------------------------
    // Expression codegen
    // -----------------------------------------------------------------------

    fn gen_expr(&mut self, expr: &Expr, span: &Span) -> Result<(), CodegenError> {
        match expr {
            Expr::Int(n) => {
                if *n >= i16::MIN as i64 && *n <= i16::MAX as i64 {
                    self.emit(Op::LoadSmallInt(*n as i16), span);
                } else {
                    let idx = self.chunk.add_constant(Value::Int(*n));
                    self.emit(Op::LoadConst(idx), span);
                }
            }
            Expr::Float(f) => {
                let idx = self.chunk.add_constant(Value::Float(*f));
                self.emit(Op::LoadConst(idx), span);
            }
            Expr::String(s) => {
                let idx = self.chunk.add_constant(Value::String(s.as_str().into()));
                self.emit(Op::LoadConst(idx), span);
            }
            Expr::Bool(b) => {
                let idx = self.chunk.add_constant(Value::Bool(*b));
                self.emit(Op::LoadConst(idx), span);
            }
            Expr::Unit => {
                let idx = self.chunk.add_constant(Value::Unit);
                self.emit(Op::LoadConst(idx), span);
            }

            Expr::Var(name, _ty) => {
                self.gen_var(name, span)?;
            }

            Expr::CallDirect { name, args, .. } => {
                self.gen_call_direct(name, args, span)?;
            }
            Expr::CallNative { module, method, args, .. } => {
                self.gen_call_native(module, method, args, span)?;
            }
            Expr::CallIndirect { callee, args, .. } => {
                self.gen_expr(callee, span)?;
                for arg in args {
                    self.gen_expr(arg, span)?;
                }
                self.emit(Op::Call(args.len() as u8), span);
            }
            Expr::TailCall { args, .. } => {
                for arg in args {
                    self.gen_expr(arg, span)?;
                }
                self.emit(Op::TailCall(args.len() as u8), span);
            }

            Expr::MakeEnum { tag, payload, .. } => {
                self.gen_expr(payload, span)?;
                let tag_idx = self.chunk.add_constant(Value::String(tag.as_str().into()));
                self.emit(Op::MakeEnum(tag_idx), span);
            }
            Expr::MakeStruct { name, fields, .. } => {
                for (key, val) in fields {
                    let key_idx = self.chunk.add_constant(Value::String(key.as_str().into()));
                    self.emit(Op::LoadConst(key_idx), span);
                    self.gen_expr(val, span)?;
                }
                self.emit(Op::MakeRecord(fields.len() as u16), span);
                let tag_idx = self.chunk.add_constant(Value::String(name.as_str().into()));
                self.emit(Op::MakeStruct(tag_idx), span);
            }

            Expr::MakeList(elems, _) => {
                for elem in elems {
                    self.gen_expr(elem, span)?;
                }
                self.emit(Op::MakeList(elems.len() as u16), span);
            }
            Expr::MakeRecord(fields, _) => {
                for (key, val) in fields {
                    let key_idx = self.chunk.add_constant(Value::String(key.as_str().into()));
                    self.emit(Op::LoadConst(key_idx), span);
                    self.gen_expr(val, span)?;
                }
                self.emit(Op::MakeRecord(fields.len() as u16), span);
            }
            Expr::MakeTuple(elems, _) => {
                for elem in elems {
                    self.gen_expr(elem, span)?;
                }
                self.emit(Op::MakeTuple(elems.len() as u16), span);
            }
            Expr::MakeRange(start, end) => {
                self.gen_expr(start, span)?;
                self.gen_expr(end, span)?;
                self.emit(Op::MakeRange, span);
            }
            Expr::UpdateRecord { base, updates, .. } => {
                self.gen_expr(base, span)?;
                for (key, val) in updates {
                    let key_idx = self.chunk.add_constant(Value::String(key.as_str().into()));
                    self.emit(Op::LoadConst(key_idx), span);
                    self.gen_expr(val, span)?;
                }
                self.emit(Op::UpdateRecord(updates.len() as u16), span);
            }

            Expr::GetField { object, field, .. } => {
                self.gen_expr(object, span)?;
                let name_idx = self.chunk.add_constant(Value::String(field.as_str().into()));
                self.emit(Op::GetField(name_idx), span);
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
                let false_const = self.chunk.add_constant(Value::Bool(false));
                self.emit(Op::LoadConst(false_const), span);
                self.chunk.patch_jump(end_idx);
            }
            Expr::Or(lhs, rhs) => {
                self.gen_expr(lhs, span)?;
                let jump_idx = self.emit(Op::JumpIfFalse(0), span);
                let true_const = self.chunk.add_constant(Value::Bool(true));
                self.emit(Op::LoadConst(true_const), span);
                let end_jump = self.emit(Op::Jump(0), span);
                self.chunk.patch_jump(jump_idx);
                self.gen_expr(rhs, span)?;
                self.chunk.patch_jump(end_jump);
            }

            Expr::If { condition, then_branch, else_branch, .. } => {
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
                    let idx = self.chunk.add_constant(Value::Unit);
                    self.emit(Op::LoadConst(idx), span);
                    self.chunk.patch_jump(else_jump);
                }
            }

            Expr::Match { subject, arms, .. } => {
                self.gen_match(subject, arms, span)?;
            }

            Expr::For { binding, iterable, body } => {
                self.gen_for(binding, iterable, body, span)?;
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

            Expr::Try { expr, .. } => {
                self.gen_try(expr, span)?;
            }

            Expr::Concat(parts) => {
                self.gen_concat(parts, span)?;
            }
        }
        Ok(())
    }

    // -----------------------------------------------------------------------
    // Variables
    // -----------------------------------------------------------------------

    fn gen_var(&mut self, name: &str, span: &Span) -> Result<(), CodegenError> {
        // Search locals
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.name == name {
                self.emit(Op::GetLocal(i as u16), span);
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
            let idx = self.chunk.add_constant(Value::Function(chunk_idx));
            self.emit(Op::LoadConst(idx), span);
            return Ok(());
        }
        Err(CodegenError {
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
            if found_at.is_some() { break; }
            for (i, uv) in self.enclosing[d].upvalues.iter().enumerate() {
                if uv.name == name {
                    found_at = Some((d, i as u16, false));
                    break;
                }
            }
            if found_at.is_some() { break; }
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

    fn gen_call_direct(&mut self, name: &str, args: &[Expr], span: &Span) -> Result<(), CodegenError> {
        if let Some(&chunk_idx) = self.functions.get(name) {
            let idx = self.chunk.add_constant(Value::Function(chunk_idx));
            self.emit(Op::LoadConst(idx), span);
            for arg in args {
                self.gen_expr(arg, span)?;
            }
            self.emit(Op::Call(args.len() as u8), span);
            Ok(())
        } else {
            // Fall back to variable lookup (might be a local/upvalue holding a function)
            self.gen_var(name, span)?;
            for arg in args {
                self.gen_expr(arg, span)?;
            }
            self.emit(Op::Call(args.len() as u8), span);
            Ok(())
        }
    }

    fn gen_call_native(&mut self, module: &str, method: &str, args: &[Expr], span: &Span) -> Result<(), CodegenError> {
        let qualified = format!("{}.{}", module, method);
        if let Some(fn_id) = self.natives.lookup(&qualified) {
            for arg in args {
                self.gen_expr(arg, span)?;
            }
            self.emit(Op::CallNative(fn_id, args.len() as u8), span);
            Ok(())
        } else {
            Err(CodegenError {
                message: format!("Unknown native function: {}", qualified),
                line: span.line,
                col: span.col,
            })
        }
    }

    // -----------------------------------------------------------------------
    // Match
    // -----------------------------------------------------------------------

    fn gen_match(&mut self, subject: &Expr, arms: &[MatchArm], span: &Span) -> Result<(), CodegenError> {
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
                    let tag_const = self.chunk.add_constant(Value::String(tag.as_str().into()));
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
            }
        }

        // Default: push Unit
        let idx = self.chunk.add_constant(Value::Unit);
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

    fn gen_for(&mut self, binding: &str, iterable: &Expr, body: &Expr, span: &Span) -> Result<(), CodegenError> {
        self.gen_expr(iterable, span)?;
        self.declare_local("__for_list");
        let list_slot = (self.locals.len() - 1) as u16;

        let zero = self.chunk.add_constant(Value::Int(0));
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
        let one = self.chunk.add_constant(Value::Int(1));
        self.emit(Op::LoadConst(one), span);
        self.emit(Op::Add, span);
        self.emit(Op::SetLocal(idx_slot), span);
        self.emit(Op::Pop, span);

        let jump_back_dist = (self.chunk.code.len() - loop_start + 1) as u16;
        self.emit(Op::JumpBack(jump_back_dist), span);

        self.chunk.patch_jump(exit_jump);

        // Cleanup
        self.locals.pop(); // __for_idx
        self.locals.pop(); // __for_list
        self.emit(Op::PopN(2), span);

        let unit = self.chunk.add_constant(Value::Unit);
        self.emit(Op::LoadConst(unit), span);

        Ok(())
    }

    // -----------------------------------------------------------------------
    // Let binding
    // -----------------------------------------------------------------------

    fn gen_let_pattern(&mut self, pattern: &Pattern, span: &Span) -> Result<(), CodegenError> {
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

    fn gen_block(&mut self, exprs: &[Expr], span: &Span) -> Result<(), CodegenError> {
        if exprs.is_empty() {
            let idx = self.chunk.add_constant(Value::Unit);
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
            let idx = self.chunk.add_constant(Value::Unit);
            self.emit(Op::LoadConst(idx), span);
        }

        self.end_scope(span);
        Ok(())
    }

    // -----------------------------------------------------------------------
    // Lambda
    // -----------------------------------------------------------------------

    fn gen_lambda(&mut self, params: &[String], body: &Expr, span: &Span) -> Result<(), CodegenError> {
        self.enter_function();

        for param in params {
            self.declare_local(param);
        }

        self.gen_expr(body, span)?;

        let (func_chunk, captured) = self.leave_function();
        let chunk_idx = self.compiled_chunks.len();
        self.compiled_chunks.push(func_chunk);

        if captured.is_empty() {
            let const_idx = self.chunk.add_constant(Value::Function(chunk_idx));
            self.emit(Op::LoadConst(const_idx), span);
        } else {
            for uv in &captured {
                if uv.is_local {
                    self.emit(Op::GetLocal(uv.index), span);
                } else {
                    self.emit(Op::GetUpvalue(uv.index as u8), span);
                }
            }
            self.emit(Op::MakeClosure(chunk_idx as u16, captured.len() as u8), span);
        }

        Ok(())
    }

    // -----------------------------------------------------------------------
    // Try expression
    // -----------------------------------------------------------------------

    fn gen_try(&mut self, expr: &Expr, span: &Span) -> Result<(), CodegenError> {
        self.gen_expr(expr, span)?;

        self.declare_local("__try_val");
        let val_slot = (self.locals.len() - 1) as u16;

        // Check Err
        self.emit(Op::GetLocal(val_slot), span);
        self.emit(Op::EnumTag, span);
        let err_tag = self.chunk.add_constant(Value::String("Err".into()));
        self.emit(Op::LoadConst(err_tag), span);
        self.emit(Op::Eq, span);
        let not_err = self.emit(Op::JumpIfFalse(0), span);
        self.emit(Op::GetLocal(val_slot), span);
        self.emit(Op::Return, span);
        self.chunk.patch_jump(not_err);

        // Check None
        self.emit(Op::GetLocal(val_slot), span);
        self.emit(Op::EnumTag, span);
        let none_tag = self.chunk.add_constant(Value::String("None".into()));
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

    fn gen_concat(&mut self, parts: &[Expr], span: &Span) -> Result<(), CodegenError> {
        if parts.is_empty() {
            let idx = self.chunk.add_constant(Value::String("".into()));
            self.emit(Op::LoadConst(idx), span);
            return Ok(());
        }

        let has_interpolation = parts.iter().any(|p| !matches!(p, Expr::String(_)));
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
            let idx = self.chunk.add_constant(Value::String("".into()));
            self.emit(Op::LoadConst(idx), span);
            self.emit(Op::Concat, span);
        }

        Ok(())
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
            self.chunk.emit(Op::CloseScope(pop_count), span.line, span.col);
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
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::lower::Lowerer;
    use crate::vm::vm::Vm;

    fn eval_via_ir(source: &str) -> Value {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_baseline::LANGUAGE.into())
            .expect("Error loading grammar");
        let tree = parser.parse(source, None).expect("Parse failed");
        let root = tree.root_node();

        let natives = NativeRegistry::new();
        let mut lowerer = Lowerer::new(source, &natives, None);
        let module = lowerer.lower_module(&root)
            .unwrap_or_else(|e| panic!("Lower error: {}", e));

        let codegen = Codegen::new(&natives);
        let mut program = codegen.generate_program(&module)
            .unwrap_or_else(|e| panic!("Codegen error: {}", e));
        program.optimize();

        let mut vm = Vm::new();
        vm.execute_program(&program).expect("VM error")
    }

    #[test]
    fn ir_pipeline_integer() {
        let source = "main : () -> Int\nmain = 42";
        assert_eq!(eval_via_ir(source), Value::Int(42));
    }

    #[test]
    fn ir_pipeline_arithmetic() {
        let source = "main : () -> Int\nmain = 1 + 2 * 3";
        assert_eq!(eval_via_ir(source), Value::Int(7));
    }

    #[test]
    fn ir_pipeline_const_fold() {
        let source = "main : () -> Int\nmain = (2 + 3) * (4 - 1)";
        assert_eq!(eval_via_ir(source), Value::Int(15));
    }

    #[test]
    fn ir_pipeline_function_call() {
        let source = "double : Int -> Int\ndouble = |x| x * 2\nmain : () -> Int\nmain = double(5)";
        assert_eq!(eval_via_ir(source), Value::Int(10));
    }

    #[test]
    fn ir_pipeline_if_else() {
        let source = "main : () -> Int\nmain = if true then 1 else 2";
        assert_eq!(eval_via_ir(source), Value::Int(1));
    }

    #[test]
    fn ir_pipeline_match() {
        let source = "main : () -> Int\nmain = match 2\n  1 -> 10\n  2 -> 20\n  _ -> 99";
        assert_eq!(eval_via_ir(source), Value::Int(20));
    }

    #[test]
    fn ir_pipeline_let_binding() {
        let source = "main : () -> Int\nmain = {\n  let x = 10\n  x + 1\n}";
        assert_eq!(eval_via_ir(source), Value::Int(11));
    }

    #[test]
    fn ir_pipeline_lambda() {
        let source = "main : () -> Int\nmain = {\n  let f = |x| x + 1\n  f(3)\n}";
        assert_eq!(eval_via_ir(source), Value::Int(4));
    }

    #[test]
    fn ir_pipeline_closure() {
        let source = "main : () -> Int\nmain = {\n  let x = 10\n  let f = |y| x + y\n  f(5)\n}";
        assert_eq!(eval_via_ir(source), Value::Int(15));
    }

    #[test]
    fn ir_pipeline_pipe() {
        let source = "double : Int -> Int\ndouble = |x| x * 2\nmain : () -> Int\nmain = 5 |> double";
        assert_eq!(eval_via_ir(source), Value::Int(10));
    }

    #[test]
    fn ir_pipeline_enum_constructor() {
        let source = "main : () -> Unknown\nmain = Some(42)";
        assert_eq!(
            eval_via_ir(source),
            Value::Enum("Some".into(), std::rc::Rc::new(Value::Int(42))),
        );
    }

    #[test]
    fn ir_pipeline_match_enum() {
        let source = "main : () -> Int\nmain = match Some(42)\n  Some(v) -> v + 1\n  None -> 0";
        assert_eq!(eval_via_ir(source), Value::Int(43));
    }

    #[test]
    fn ir_pipeline_string_interpolation() {
        let source = "main : () -> String\nmain = \"count: ${42}\"";
        assert_eq!(eval_via_ir(source), Value::String("count: 42".into()));
    }

    #[test]
    fn ir_pipeline_recursive() {
        let source = "\
factorial : Int -> Int
factorial = |n| if n <= 1 then 1 else n * factorial(n - 1)
main : () -> Int
main = factorial(5)";
        assert_eq!(eval_via_ir(source), Value::Int(120));
    }

    #[test]
    fn ir_pipeline_tco() {
        let source = "\
countdown : Int -> Int
countdown = |n| if n <= 0 then 0 else countdown(n - 1)
main : () -> Int
main = countdown(50000)";
        assert_eq!(eval_via_ir(source), Value::Int(0));
    }

    #[test]
    fn ir_pipeline_list() {
        let source = "main : () -> Unknown\nmain = [1, 2, 3]";
        assert_eq!(
            eval_via_ir(source),
            Value::List(std::rc::Rc::new(vec![Value::Int(1), Value::Int(2), Value::Int(3)])),
        );
    }

    #[test]
    fn ir_pipeline_record() {
        let source = "main : () -> Unknown\nmain = { x: 1, y: 2 }";
        assert_eq!(
            eval_via_ir(source),
            Value::Record(std::rc::Rc::new(vec![
                ("x".into(), Value::Int(1)),
                ("y".into(), Value::Int(2)),
            ])),
        );
    }

    #[test]
    fn ir_pipeline_native_call() {
        let source = "main : () -> Int\nmain = String.length(\"hello\")";
        assert_eq!(eval_via_ir(source), Value::Int(5));
    }

    #[test]
    fn ir_pipeline_pipe_native() {
        let source = "main : () -> Int\nmain = \"hello\" |> String.length";
        assert_eq!(eval_via_ir(source), Value::Int(5));
    }

    #[test]
    fn ir_pipeline_try_ok() {
        let source = "main : () -> Unknown\nmain = {\n  let x = Ok(42)\n  let v = x?\n  v + 1\n}";
        assert_eq!(eval_via_ir(source), Value::Int(43));
    }

    #[test]
    fn ir_pipeline_range() {
        let source = "main : () -> Unknown\nmain = 1..4";
        assert_eq!(
            eval_via_ir(source),
            Value::List(std::rc::Rc::new(vec![Value::Int(1), Value::Int(2), Value::Int(3)])),
        );
    }

    #[test]
    fn ir_pipeline_record_update() {
        let source = "main : () -> Unknown\nmain = {\n  let r = { x: 1, y: 2 }\n  { ..r, x: 10 }\n}";
        assert_eq!(
            eval_via_ir(source),
            Value::Record(std::rc::Rc::new(vec![
                ("x".into(), Value::Int(10)),
                ("y".into(), Value::Int(2)),
            ])),
        );
    }

    #[test]
    fn ir_pipeline_bool_and() {
        let source = "main : () -> Boolean\nmain = true && false";
        assert_eq!(eval_via_ir(source), Value::Bool(false));
    }

    #[test]
    fn ir_pipeline_bool_or() {
        let source = "main : () -> Boolean\nmain = false || true";
        assert_eq!(eval_via_ir(source), Value::Bool(true));
    }

    #[test]
    fn ir_pipeline_for_loop() {
        let source = "main : () -> Unknown\nmain = for x in 1..4 do x + 1";
        assert_eq!(eval_via_ir(source), Value::Unit);
    }

    #[test]
    fn ir_pipeline_tuple() {
        let source = "main : () -> Unknown\nmain = (1, 2, 3)";
        assert_eq!(
            eval_via_ir(source),
            Value::Tuple(std::rc::Rc::new(vec![Value::Int(1), Value::Int(2), Value::Int(3)])),
        );
    }

    #[test]
    fn ir_pipeline_unit() {
        let source = "main : () -> Unknown\nmain = ()";
        assert_eq!(eval_via_ir(source), Value::Unit);
    }

    #[test]
    fn ir_pipeline_nested_closures() {
        let source = "\
make_adder : Int -> (Int -> Int)
make_adder = |n| |x| n + x
main : () -> Int
main = {
  let add5 = make_adder(5)
  add5(3)
}";
        assert_eq!(eval_via_ir(source), Value::Int(8));
    }

    #[test]
    fn ir_pipeline_hof_list_map() {
        let source = "main : () -> Unknown\nmain = List.map([1, 2, 3], |x| x * 2)";
        assert_eq!(
            eval_via_ir(source),
            Value::List(std::rc::Rc::new(vec![Value::Int(2), Value::Int(4), Value::Int(6)])),
        );
    }

    #[test]
    fn ir_pipeline_pipe_with_args() {
        let source = "add : (Int, Int) -> Int\nadd = |a, b| a + b\nmain : () -> Int\nmain = 3 |> add(4)";
        assert_eq!(eval_via_ir(source), Value::Int(7));
    }
}
