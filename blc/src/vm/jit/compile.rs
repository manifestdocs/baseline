//! Function compilation context and expression codegen.
//!
//! `FnCompileCtx` holds per-function state during JIT compilation and provides
//! methods for compiling IR expressions to Cranelift IR.

use std::collections::{HashMap, HashSet};

use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::{
    AbiParam, BlockArg, InstBuilder, MemFlags, StackSlotData, StackSlotKind, types,
};
use cranelift_codegen::isa::CallConv;
use cranelift_frontend::{FunctionBuilder, Variable};
use cranelift_module::{DataId, FuncId, Module};

use super::super::ir::{BinOp, Expr, IrFunction, MatchArm, Matcher, Pattern, TagRegistry, UnaryOp};
use super::super::natives::NativeRegistry;
use super::super::nvalue::{NValue, PAYLOAD_MASK, TAG_BOOL, TAG_INT};
use super::analysis::expr_only_refs_params;
use super::helpers::{NV_FALSE, NV_TRUE, NV_UNIT};
use crate::analysis::types::Type;
use baseline_rt::value::RcStr;

// ---------------------------------------------------------------------------
// Function compile context
// ---------------------------------------------------------------------------

pub(super) struct FnCompileCtx<'a, 'b, M: Module> {
    pub(super) builder: &'a mut FunctionBuilder<'b>,
    pub(super) func_ids: &'a [Option<FuncId>],
    pub(super) module: &'a mut M,
    pub(super) vars: HashMap<String, Variable>,
    pub(super) func_names: &'a HashMap<String, usize>,
    pub(super) ir_functions: &'a [IrFunction],
    pub(super) current_func_name: String,
    pub(super) param_vars: Vec<Variable>,
    pub(super) loop_header: Option<cranelift_codegen::ir::Block>,
    pub(super) heap_roots: &'a mut Vec<NValue>,
    pub(super) helper_ids: &'a HashMap<&'a str, FuncId>,
    pub(super) natives: Option<&'a NativeRegistry>,
    pub(super) ptr_type: cranelift_codegen::ir::Type,
    /// Per-function unboxed flags (indexed by function index).
    pub(super) unboxed_flags: &'a [bool],
    /// Compile-time enum tag → integer ID mapping.
    pub(super) tags: &'a TagRegistry,
    /// SRA: record variables that have been scalar-replaced.
    /// Maps record variable name → (field name → Cranelift Variable).
    pub(super) sra_records: HashMap<String, HashMap<String, Variable>>,
    /// Original param SRA vars — preserved for tail call loop variable rebinding.
    /// Maps param name → (field_name → Variable). Never modified after seeding.
    pub(super) param_sra_original: HashMap<String, HashMap<String, Variable>>,
    /// Variables that should re-seed SRA field caches when rebound.
    pub(super) sra_hot_vars: HashSet<String>,
    /// AOT string constants: maps string content → DataId in the object module.
    /// When Some, strings are loaded from global data instead of baked as heap pointers.
    pub(super) aot_strings: Option<&'a HashMap<String, DataId>>,
    /// AOT native function IDs: maps qualified name → FuncId for direct calls.
    /// When Some, CallNative emits direct calls instead of going through NativeRegistry.
    pub(super) aot_native_ids: Option<&'a HashMap<String, FuncId>>,

    /// Reusable scratch stack slot for spill_to_stack (slot, size in bytes).
    pub(super) scratch_slot: Option<(cranelift_codegen::ir::StackSlot, u32)>,
    /// Whether RC codegen is enabled (scope-based incref/decref).
    pub(super) rc_enabled: bool,
    /// Stack of scope frames tracking owned variables for RC cleanup.
    /// Each frame is a list of Variables whose values should be decref'd on scope exit.
    pub(super) rc_scope_stack: Vec<Vec<Variable>>,
    /// Calling convention for Baseline functions (Tail for JIT, Fast for AOT).
    pub(super) func_call_conv: CallConv,
    /// Multi-return: sorted field names for functions returning all-scalar records.
    /// When set, the function uses N return registers instead of a single boxed record.
    pub(super) multireturn_fields: Option<Vec<String>>,
    /// Multi-return info for all functions (indexed by function index).
    /// Used at call sites to detect multi-return callees.
    pub(super) multireturn_info: &'a [Option<Vec<String>>],
}

pub(super) type CValue = cranelift_codegen::ir::Value;

impl<'a, 'b, M: Module> FnCompileCtx<'a, 'b, M> {
    pub(super) fn new_var(&mut self) -> Variable {
        self.builder.declare_var(types::I64)
    }

    /// Stash an SSA value into a Cranelift Variable so it remains valid
    /// across block boundaries created by subsequent compile_expr calls.
    pub(super) fn stash_in_var(&mut self, val: CValue) -> CValue {
        let var = self.new_var();
        self.builder.def_var(var, val);
        self.builder.use_var(var)
    }

    /// Compute sorted field index for a field name given a record/struct type.
    fn sorted_field_index(ty: &Type, field_name: &str) -> Option<u16> {
        let fields = match ty {
            Type::Struct(_, fields) | Type::Record(fields, _) => fields,
            _ => return None,
        };
        let mut names: Vec<&str> = fields.keys().map(|s| s.as_str()).collect();
        names.sort();
        names
            .iter()
            .position(|&n| n == field_name)
            .map(|i| i as u16)
    }

    /// Best-effort check for expressions that are statically known to produce Float.
    /// Used for native intrinsics that are only safe on raw float bit-patterns.
    fn expr_is_known_float(expr: &Expr) -> bool {
        matches!(
            expr,
            Expr::Float(_)
                | Expr::Var(_, Some(Type::Float))
                | Expr::CallDirect {
                    ty: Some(Type::Float),
                    ..
                }
                | Expr::CallNative {
                    ty: Some(Type::Float),
                    ..
                }
                | Expr::CallIndirect {
                    ty: Some(Type::Float),
                    ..
                }
                | Expr::TailCall {
                    ty: Some(Type::Float),
                    ..
                }
                | Expr::TailCallIndirect {
                    ty: Some(Type::Float),
                    ..
                }
                | Expr::GetField {
                    ty: Some(Type::Float),
                    ..
                }
                | Expr::BinOp {
                    ty: Some(Type::Float),
                    ..
                }
                | Expr::UnaryOp {
                    ty: Some(Type::Float),
                    ..
                }
                | Expr::If {
                    ty: Some(Type::Float),
                    ..
                }
                | Expr::Match {
                    ty: Some(Type::Float),
                    ..
                }
                | Expr::Try {
                    ty: Some(Type::Float),
                    ..
                }
                | Expr::Block(_, Some(Type::Float))
        )
    }

    fn is_scalar_field_type(ty: &Type) -> bool {
        matches!(ty, Type::Int | Type::Float | Type::Bool | Type::Unit)
    }

    fn expr_type_hint(expr: &Expr) -> Option<Type> {
        match expr {
            Expr::Int(_) => Some(Type::Int),
            Expr::Float(_) => Some(Type::Float),
            Expr::String(_) => Some(Type::String),
            Expr::Bool(_) => Some(Type::Bool),
            Expr::Unit => Some(Type::Unit),
            Expr::Var(_, ty)
            | Expr::CallDirect { ty, .. }
            | Expr::CallNative { ty, .. }
            | Expr::CallIndirect { ty, .. }
            | Expr::TailCall { ty, .. }
            | Expr::TailCallIndirect { ty, .. }
            | Expr::MakeEnum { ty, .. }
            | Expr::MakeStruct { ty, .. }
            | Expr::UpdateRecord { ty, .. }
            | Expr::GetField { ty, .. }
            | Expr::BinOp { ty, .. }
            | Expr::UnaryOp { ty, .. }
            | Expr::If { ty, .. }
            | Expr::Match { ty, .. }
            | Expr::Let { ty, .. }
            | Expr::Lambda { ty, .. }
            | Expr::Try { ty, .. }
            | Expr::PerformEffect { ty, .. }
            | Expr::Block(_, ty)
            | Expr::MakeList(_, ty)
            | Expr::MakeTuple(_, ty)
            | Expr::MakeRecord(_, ty) => ty.clone(),
            Expr::For { .. } => Some(Type::Unit),
            Expr::And(_, _) | Expr::Or(_, _) => Some(Type::Bool),
            Expr::MakeRange(_, _) => Some(Type::List(Box::new(Type::Int))),
            Expr::Concat(_) => Some(Type::String),
            Expr::Hole
            | Expr::GetClosureVar(_)
            | Expr::MakeClosure { .. }
            | Expr::WithHandlers { .. }
            | Expr::HandleEffect { .. }
            | Expr::Expect { .. }
            | Expr::Drop { .. }
            | Expr::Reuse { .. }
            | Expr::Assign { .. }
            | Expr::FieldAssign { .. } => None,
        }
    }

    fn seed_record_field_cache(
        &mut self,
        var_name: &str,
        fields: &HashMap<String, Type>,
    ) -> Result<(), String> {
        let Some(&base_var) = self.vars.get(var_name) else {
            return Ok(());
        };
        let base_val = self.builder.use_var(base_var);

        let mut sorted_fields: Vec<_> = fields.iter().collect();
        sorted_fields.sort_by(|a, b| a.0.cmp(b.0));

        let mut field_vars = HashMap::new();
        for (field_idx, (field_name, field_ty)) in sorted_fields.iter().enumerate() {
            if !Self::is_scalar_field_type(field_ty) {
                continue;
            }
            let idx_val = self.builder.ins().iconst(types::I64, field_idx as i64);
            let field_val = self.call_helper("jit_get_field_idx", &[base_val, idx_val]);
            let field_var = self.new_var();
            self.builder.def_var(field_var, field_val);
            if self.rc_enabled {
                self.rc_track_var(field_var);
            }
            field_vars.insert((*field_name).clone(), field_var);
        }

        if !field_vars.is_empty() {
            self.sra_records.insert(var_name.to_string(), field_vars);
        }
        Ok(())
    }

    fn seed_hot_var_cache_from_type(&mut self, var_name: &str, ty: &Type) -> Result<(), String> {
        let mut current = ty;
        while let Type::Refined(inner, _) = current {
            current = inner;
        }
        match current {
            Type::Struct(_, fields) | Type::Record(fields, _) => {
                self.seed_record_field_cache(var_name, fields)
            }
            _ => Ok(()),
        }
    }

    fn reseed_hot_pattern_vars(&mut self, pattern: &Pattern, ty: &Type) -> Result<(), String> {
        match pattern {
            Pattern::Var(name) => {
                if self.sra_hot_vars.contains(name.as_str()) {
                    self.seed_hot_var_cache_from_type(name, ty)?;
                }
                Ok(())
            }
            Pattern::Tuple(items) => {
                if let Type::Tuple(item_tys) = ty {
                    for (pat, item_ty) in items.iter().zip(item_tys.iter()) {
                        self.reseed_hot_pattern_vars(pat, item_ty)?;
                    }
                }
                Ok(())
            }
            Pattern::List(items, rest) => {
                if let Type::List(inner) = ty {
                    for pat in items {
                        self.reseed_hot_pattern_vars(pat, inner)?;
                    }
                    if let Some(rest_name) = rest.as_ref()
                        && self.sra_hot_vars.contains(rest_name.as_str())
                    {
                        // Rest binding is still a list of the same element type.
                        self.seed_hot_var_cache_from_type(rest_name, ty)?;
                    }
                }
                Ok(())
            }
            Pattern::Constructor(tag, payloads) => {
                if let Type::Enum(_, variants) = ty {
                    let payload_ty = variants.iter().find_map(|(variant_name, fields)| {
                        if variant_name == tag && fields.len() == payloads.len() {
                            Some(fields)
                        } else {
                            None
                        }
                    });
                    if let Some(payload_tys) = payload_ty {
                        for (pat, field_ty) in payloads.iter().zip(payload_tys.iter()) {
                            self.reseed_hot_pattern_vars(pat, field_ty)?;
                        }
                    }
                }
                Ok(())
            }
            Pattern::Record(fields) => {
                let mut current = ty;
                while let Type::Refined(inner, _) = current {
                    current = inner;
                }
                if let Type::Struct(_, rec_fields) | Type::Record(rec_fields, _) = current {
                    for (field_name, pat) in fields {
                        if let Some(field_ty) = rec_fields.get(field_name) {
                            self.reseed_hot_pattern_vars(pat, field_ty)?;
                        }
                    }
                }
                Ok(())
            }
            Pattern::Literal(_) | Pattern::Wildcard => Ok(()),
        }
    }

    fn alias_sra_pattern_bindings(&mut self, pattern: &Pattern, value: &Expr) {
        if let (Pattern::Var(dst), Expr::Var(src, _)) = (pattern, value)
            && let Some(src_fields) = self.sra_records.get(src.as_str()).cloned()
        {
            self.sra_records.insert(dst.clone(), src_fields);
            self.sra_hot_vars.insert(dst.clone());
        }
    }

    fn count_matcher_param_field_reads(matcher: &Matcher, param_name: &str) -> usize {
        match matcher {
            Matcher::Equal(expected)
            | Matcher::HaveLength(expected)
            | Matcher::Contain(expected)
            | Matcher::StartWith(expected)
            | Matcher::Satisfy(expected) => Self::count_param_field_reads(expected, param_name),
            Matcher::BeOk | Matcher::BeSome | Matcher::BeNone | Matcher::BeEmpty => 0,
            Matcher::Be(_pattern) => 0,
        }
    }

    fn count_param_field_reads(expr: &Expr, param_name: &str) -> usize {
        match expr {
            Expr::GetField { object, .. } => {
                let this_read = usize::from(
                    matches!(object.as_ref(), Expr::Var(name, _) if name == param_name),
                );
                this_read + Self::count_param_field_reads(object, param_name)
            }
            Expr::BinOp { lhs, rhs, .. }
            | Expr::And(lhs, rhs)
            | Expr::Or(lhs, rhs)
            | Expr::MakeRange(lhs, rhs) => {
                Self::count_param_field_reads(lhs, param_name)
                    + Self::count_param_field_reads(rhs, param_name)
            }
            Expr::UnaryOp { operand, .. } => Self::count_param_field_reads(operand, param_name),
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                Self::count_param_field_reads(condition, param_name)
                    + Self::count_param_field_reads(then_branch, param_name)
                    + else_branch
                        .as_ref()
                        .map_or(0, |e| Self::count_param_field_reads(e, param_name))
            }
            Expr::CallDirect { args, .. }
            | Expr::CallNative { args, .. }
            | Expr::TailCall { args, .. } => args
                .iter()
                .map(|a| Self::count_param_field_reads(a, param_name))
                .sum(),
            Expr::CallIndirect { callee, args, .. }
            | Expr::TailCallIndirect { callee, args, .. } => {
                Self::count_param_field_reads(callee, param_name)
                    + args
                        .iter()
                        .map(|a| Self::count_param_field_reads(a, param_name))
                        .sum::<usize>()
            }
            Expr::Let { value, .. } | Expr::Assign { value, .. } | Expr::FieldAssign { value, .. } => Self::count_param_field_reads(value, param_name),
            Expr::Block(exprs, _) | Expr::MakeList(exprs, _) | Expr::MakeTuple(exprs, _) => exprs
                .iter()
                .map(|e| Self::count_param_field_reads(e, param_name))
                .sum(),
            Expr::Concat(parts) => parts
                .iter()
                .map(|p| Self::count_param_field_reads(p, param_name))
                .sum(),
            Expr::Match { subject, arms, .. } => {
                let mut total = Self::count_param_field_reads(subject, param_name);
                for arm in arms {
                    total += Self::count_param_field_reads(&arm.body, param_name);
                    if let Some(g) = &arm.guard {
                        total += Self::count_param_field_reads(g, param_name);
                    }
                }
                total
            }
            Expr::MakeEnum { payload, .. } => Self::count_param_field_reads(payload, param_name),
            Expr::MakeStruct { fields, .. } | Expr::MakeRecord(fields, _) => fields
                .iter()
                .map(|(_, v)| Self::count_param_field_reads(v, param_name))
                .sum(),
            Expr::UpdateRecord { base, updates, .. } => {
                Self::count_param_field_reads(base, param_name)
                    + updates
                        .iter()
                        .map(|(_, v)| Self::count_param_field_reads(v, param_name))
                        .sum::<usize>()
            }
            Expr::For { iterable, body, .. } => {
                Self::count_param_field_reads(iterable, param_name)
                    + Self::count_param_field_reads(body, param_name)
            }
            Expr::Try { expr, .. } => Self::count_param_field_reads(expr, param_name),
            Expr::MakeClosure { captures, .. } => captures
                .iter()
                .map(|c| Self::count_param_field_reads(c, param_name))
                .sum(),
            Expr::Lambda { body, .. } => Self::count_param_field_reads(body, param_name),
            Expr::WithHandlers { handlers, body, .. } => {
                let mut total = Self::count_param_field_reads(body, param_name);
                for (_, methods) in handlers {
                    for (_, h) in methods {
                        total += Self::count_param_field_reads(h, param_name);
                    }
                }
                total
            }
            Expr::HandleEffect { body, clauses, .. } => {
                let mut total = Self::count_param_field_reads(body, param_name);
                for clause in clauses {
                    total += Self::count_param_field_reads(&clause.body, param_name);
                }
                total
            }
            Expr::PerformEffect { args, .. } => args
                .iter()
                .map(|a| Self::count_param_field_reads(a, param_name))
                .sum(),
            Expr::Expect { actual, matcher } => {
                Self::count_param_field_reads(actual, param_name)
                    + Self::count_matcher_param_field_reads(matcher, param_name)
            }
            Expr::Drop { body, .. } => Self::count_param_field_reads(body, param_name),
            Expr::Reuse { alloc, .. } => Self::count_param_field_reads(alloc, param_name),
            Expr::Int(_)
            | Expr::Float(_)
            | Expr::String(_)
            | Expr::Bool(_)
            | Expr::Unit
            | Expr::Hole
            | Expr::Var(_, _)
            | Expr::GetClosureVar(_) => 0,
        }
    }

    /// Seed SRA field caches for struct/record parameters when profitable.
    ///
    /// This reduces repeated `GetField` helper calls by extracting hot parameter
    /// fields once at function entry and serving reads from SSA variables.
    ///
    /// For tail-recursive functions, ALL record params are SRA'd regardless of
    /// profitability. This enables SRA-aware tail calls: instead of materializing
    /// records and re-extracting fields on each loop iteration, field variables
    /// are directly rebound at the tail call site.
    pub(super) fn seed_param_field_cache(
        &mut self,
        func_ty: Option<&Type>,
        params: &[String],
        param_types: &[Option<Type>],
        body: &Expr,
        is_tail_recursive: bool,
    ) -> Result<(), String> {
        // Get param types: prefer explicit param_types from IR, fall back to
        // function type signature, then infer from body Var references.
        let param_tys_from_sig: Vec<Option<Type>> = if !param_types.is_empty() {
            param_types.to_vec()
        } else if let Some(Type::Function(pts, _)) = func_ty {
            params
                .iter()
                .enumerate()
                .map(|(i, _)| pts.get(i).cloned())
                .collect()
        } else {
            // Infer from body: find Var(name, Some(type)) references
            let inferred = Self::infer_param_types(body, params);
            params
                .iter()
                .map(|name| inferred.get(name.as_str()).cloned())
                .collect()
        };

        for (idx, param_name) in params.iter().enumerate() {
            let Some(param_ty) = param_tys_from_sig.get(idx).and_then(|t| t.as_ref()) else {
                continue;
            };
            let fields = match param_ty {
                Type::Struct(_, fields) | Type::Record(fields, _) => fields,
                _ => continue,
            };

            // For tail-recursive functions, always SRA record params to enable
            // zero-allocation tail calls. Otherwise, only cache when profitable.
            if !is_tail_recursive {
                let read_count = Self::count_param_field_reads(body, param_name);
                if read_count <= fields.len() {
                    continue;
                }
            }

            self.seed_record_field_cache(param_name, fields)?;
            self.sra_hot_vars.insert(param_name.clone());

            // For tail-recursive functions, save the original param SRA vars
            // and remove from vars so reads go through SRA (not boxed param).
            if is_tail_recursive {
                if let Some(sra) = self.sra_records.get(param_name) {
                    self.param_sra_original
                        .insert(param_name.clone(), sra.clone());
                }
                self.vars.remove(param_name);
            }
        }
        Ok(())
    }

    /// Infer parameter types from Var references in the function body.
    /// Scans for Var(name, Some(type)) where name matches a param.
    fn infer_param_types<'e>(
        body: &'e Expr,
        params: &[String],
    ) -> HashMap<&'e str, Type> {
        let mut result = HashMap::new();
        Self::collect_var_types(body, params, &mut result);
        result
    }

    fn collect_var_types<'e>(
        expr: &'e Expr,
        params: &[String],
        out: &mut HashMap<&'e str, Type>,
    ) {
        match expr {
            Expr::Var(name, Some(ty)) if params.iter().any(|p| p == name) => {
                out.entry(name.as_str()).or_insert_with(|| ty.clone());
            }
            Expr::GetField { object, .. } => Self::collect_var_types(object, params, out),
            Expr::BinOp { lhs, rhs, .. }
            | Expr::And(lhs, rhs)
            | Expr::Or(lhs, rhs) => {
                Self::collect_var_types(lhs, params, out);
                Self::collect_var_types(rhs, params, out);
            }
            Expr::UnaryOp { operand, .. } => Self::collect_var_types(operand, params, out),
            Expr::If { condition, then_branch, else_branch, .. } => {
                Self::collect_var_types(condition, params, out);
                Self::collect_var_types(then_branch, params, out);
                if let Some(e) = else_branch {
                    Self::collect_var_types(e, params, out);
                }
            }
            Expr::Let { value, .. } | Expr::Assign { value, .. } | Expr::FieldAssign { value, .. } => {
                Self::collect_var_types(value, params, out);
            }
            Expr::Block(exprs, _) => {
                for e in exprs {
                    Self::collect_var_types(e, params, out);
                }
            }
            Expr::CallDirect { args, .. }
            | Expr::TailCall { args, .. }
            | Expr::CallNative { args, .. } => {
                for a in args {
                    Self::collect_var_types(a, params, out);
                }
            }
            Expr::CallIndirect { callee, args, .. }
            | Expr::TailCallIndirect { callee, args, .. } => {
                Self::collect_var_types(callee, params, out);
                for a in args {
                    Self::collect_var_types(a, params, out);
                }
            }
            Expr::Match { subject, arms, .. } => {
                Self::collect_var_types(subject, params, out);
                for arm in arms {
                    Self::collect_var_types(&arm.body, params, out);
                }
            }
            Expr::MakeRecord(fields, _) | Expr::MakeStruct { fields, .. } => {
                for (_, v) in fields {
                    Self::collect_var_types(v, params, out);
                }
            }
            Expr::UpdateRecord { base, updates, .. } => {
                Self::collect_var_types(base, params, out);
                for (_, v) in updates {
                    Self::collect_var_types(v, params, out);
                }
            }
            Expr::MakeTuple(items, _) | Expr::MakeList(items, _) => {
                for e in items {
                    Self::collect_var_types(e, params, out);
                }
            }
            Expr::MakeEnum { payload, .. } => Self::collect_var_types(payload, params, out),
            Expr::MakeClosure { captures, .. } => {
                for c in captures {
                    Self::collect_var_types(c, params, out);
                }
            }
            Expr::Try { expr, .. } => Self::collect_var_types(expr, params, out),
            Expr::Concat(parts) => {
                for p in parts {
                    Self::collect_var_types(p, params, out);
                }
            }
            Expr::For { iterable, body, .. } => {
                Self::collect_var_types(iterable, params, out);
                Self::collect_var_types(body, params, out);
            }
            Expr::Drop { body, .. } => Self::collect_var_types(body, params, out),
            Expr::Reuse { alloc, .. } => Self::collect_var_types(alloc, params, out),
            _ => {}
        }
    }

    /// Compile a slice of expressions, stashing each intermediate result
    /// in a Variable to prevent SSA dominance violations when later
    /// compile_expr calls create new blocks (if/else, match, etc.).
    fn compile_exprs_stashed(&mut self, exprs: &[Expr]) -> Result<Vec<CValue>, String> {
        let mut vals = Vec::with_capacity(exprs.len());
        for (i, e) in exprs.iter().enumerate() {
            let val = self.compile_expr(e)?;
            // Stash all but the last value (the last is always in the current block)
            if i < exprs.len() - 1 {
                vals.push(self.stash_in_var(val));
            } else {
                vals.push(val);
            }
        }
        Ok(vals)
    }

    // -- RC scope helpers --

    /// Push a new RC scope frame. Variables tracked in this frame will be
    /// decref'd when the scope is popped.
    pub(super) fn push_rc_scope(&mut self) {
        if self.rc_enabled {
            self.rc_scope_stack.push(Vec::new());
        }
    }

    /// Track a variable as owned in the current RC scope.
    pub(super) fn rc_track_var(&mut self, var: Variable) {
        if self.rc_enabled
            && let Some(frame) = self.rc_scope_stack.last_mut()
        {
            frame.push(var);
        }
    }

    /// Pop an RC scope frame, emitting decref for all tracked variables
    /// except `keep` (the return value variable, if any).
    pub(super) fn pop_rc_scope(&mut self, keep: Option<Variable>) {
        if !self.rc_enabled {
            return;
        }
        if let Some(frame) = self.rc_scope_stack.pop() {
            for var in &frame {
                if keep == Some(*var) {
                    continue;
                }
                let val = self.builder.use_var(*var);
                self.emit_decref(val);
            }
        }
    }

    /// Emit an incref call. Returns the same value (for chaining).
    pub(super) fn emit_incref(&mut self, val: CValue) -> CValue {
        self.call_helper("jit_rc_incref", &[val])
    }

    /// Emit a decref call for a NaN-boxed value.
    /// jit_rc_decref checks the tag internally and is a no-op for non-heap values.
    pub(super) fn emit_decref(&mut self, val: CValue) {
        self.call_helper_void("jit_rc_decref", &[val]);
    }

    /// Call a runtime helper that returns no useful value.
    pub(super) fn call_helper_void(&mut self, name: &str, args: &[CValue]) {
        let func_id = self.helper_ids[name];
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        self.builder.ins().call(func_ref, args);
    }

    // -- NaN-boxing helpers --

    /// Emit a NaN-boxed NValue constant.
    fn emit_nvalue(&mut self, nv: NValue) -> CValue {
        if nv.is_heap() {
            // BigInt, or any other heap-allocated value: must keep rooted
            return self.emit_heap_nvalue(nv);
        }
        let bits = nv.raw() as i64;
        self.builder.ins().iconst(types::I64, bits)
    }

    /// Emit a NaN-boxed NValue for a heap object, keeping it rooted.
    pub(super) fn emit_heap_nvalue(&mut self, nv: NValue) -> CValue {
        let bits = nv.raw() as i64;
        self.heap_roots.push(nv);
        let val = self.builder.ins().iconst(types::I64, bits);
        // RC: heap_roots owns one ref; incref so the function gets its own.
        if self.rc_enabled {
            self.emit_incref(val)
        } else {
            val
        }
    }

    /// Emit a string NValue: AOT uses global data + jit_make_string, JIT bakes the pointer.
    fn emit_string_value(&mut self, s: &str) -> Result<CValue, String> {
        if let Some(aot_strings) = self.aot_strings {
            let data_id = aot_strings
                .get(s)
                .ok_or_else(|| format!("String not in AOT data: {:?}", s))?;
            let gv = self
                .module
                .declare_data_in_func(*data_id, self.builder.func);
            let addr = self.builder.ins().global_value(self.ptr_type, gv);
            let len = self.builder.ins().iconst(types::I64, s.len() as i64);
            Ok(self.call_helper("jit_make_string", &[addr, len]))
        } else {
            // JIT path: allocate a fresh string NValue for each occurrence.
            //
            // NOTE: A previous string pool optimization cached Variables to
            // deduplicate allocations. However, this was unsound: def_var in
            // one match arm block doesn't dominate sibling arms, so use_var
            // returns 0x0 (the Cranelift default for undefined Variables).
            // This caused MakeEnum to receive a null tag string, silently
            // producing Unit instead of the correct enum value.
            //
            // If pooling is reintroduced, pooled variables must be initialized in
            // a dominating block (e.g., function entry) before any control-flow split.
            let nv = NValue::string(s.into());
            Ok(self.emit_heap_nvalue(nv))
        }
    }

    /// Untag an Int: extract the 48-bit signed payload from NaN-boxed int.
    /// ishl 16, sshr 16
    pub(super) fn untag_int(&mut self, val: CValue) -> CValue {
        let shifted_left = self.builder.ins().ishl_imm(val, 16);
        self.builder.ins().sshr_imm(shifted_left, 16)
    }

    /// Bitcast a NaN-boxed float (I64 raw bits) to F64 for Cranelift float ops.
    /// This is a no-op at the machine level — NaN-boxed floats ARE raw IEEE 754 bits.
    fn bitcast_to_f64(&mut self, val: CValue) -> CValue {
        self.builder.ins().bitcast(types::F64, MemFlags::new(), val)
    }

    /// Bitcast an F64 back to I64 (NaN-boxed float representation).
    fn bitcast_to_i64(&mut self, val: CValue) -> CValue {
        self.builder.ins().bitcast(types::I64, MemFlags::new(), val)
    }

    /// Tag a raw i64 as a NaN-boxed Int: band with PAYLOAD_MASK, bor with TAG_INT.
    /// Only correct when the value is guaranteed to fit in 48-bit signed range.
    pub(super) fn tag_int(&mut self, val: CValue) -> CValue {
        let mask = self.builder.ins().iconst(types::I64, PAYLOAD_MASK as i64);
        let masked = self.builder.ins().band(val, mask);
        let tag = self.builder.ins().iconst(types::I64, TAG_INT as i64);
        self.builder.ins().bor(masked, tag)
    }

    /// Tag an i64 arithmetic result with overflow checking.
    /// If the value fits in 48-bit signed range, tags inline (fast path).
    /// Otherwise calls jit_int_from_i64 to allocate a BigInt (slow path).
    fn tag_int_checked(&mut self, val: CValue) -> CValue {
        // Sign-extend from 48 bits and compare with original
        let sext = self.builder.ins().ishl_imm(val, 16);
        let sext = self.builder.ins().sshr_imm(sext, 16);
        let fits = self.builder.ins().icmp(IntCC::Equal, val, sext);

        let inline_block = self.builder.create_block();
        let overflow_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);

        self.builder
            .ins()
            .brif(fits, inline_block, &[], overflow_block, &[]);

        self.builder.switch_to_block(inline_block);
        self.builder.seal_block(inline_block);
        let fast = self.tag_int(val);
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(fast)]);

        self.builder.switch_to_block(overflow_block);
        self.builder.seal_block(overflow_block);
        let slow = self.call_helper("jit_int_from_i64", &[val]);
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(slow)]);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        self.builder.block_params(merge_block)[0]
    }

    /// Tag a Cranelift i8 comparison result as a NaN-boxed Bool.
    fn tag_bool(&mut self, cmp: CValue) -> CValue {
        let extended = self.builder.ins().uextend(types::I64, cmp);
        let tag = self.builder.ins().iconst(types::I64, TAG_BOOL as i64);
        self.builder.ins().bor(extended, tag)
    }

    /// Check if a NaN-boxed value is truthy (bit 0 set, works for NaN-boxed bools).
    pub(super) fn is_truthy(&mut self, val: CValue) -> CValue {
        let one = self.builder.ins().iconst(types::I64, 1);
        let bit = self.builder.ins().band(val, one);
        let zero = self.builder.ins().iconst(types::I64, 0);
        self.builder.ins().icmp(IntCC::NotEqual, bit, zero)
    }

    /// Spill values to a stack slot and return the address.
    /// Reuses a scratch slot when possible to avoid creating many stack slots.
    fn spill_to_stack(&mut self, values: &[CValue]) -> CValue {
        let size = (values.len() * 8) as u32;
        let slot = match self.scratch_slot {
            Some((existing, existing_size)) if existing_size >= size => existing,
            _ => {
                let s = self.builder.create_sized_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    size,
                    3, // 8-byte alignment
                ));
                self.scratch_slot = Some((s, size));
                s
            }
        };
        for (i, &v) in values.iter().enumerate() {
            let offset = (i * 8) as i32;
            self.builder.ins().stack_store(v, slot, offset);
        }
        self.builder.ins().stack_addr(self.ptr_type, slot, 0)
    }

    /// Call a runtime helper function by name.
    pub(super) fn call_helper(&mut self, name: &str, args: &[CValue]) -> CValue {
        let func_id = self.helper_ids[name];
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        let call = self.builder.ins().call(func_ref, args);
        self.builder.inst_results(call)[0]
    }

    // -- Base-case speculation --

    /// Check if an expression is simple enough to inline for base-case speculation.
    /// Recognizes scalars, variables, and binary/unary ops over simple operands
    /// (e.g., `a + b`, `n - 1`).
    fn is_simple_base_case(e: &Expr) -> bool {
        match e {
            Expr::Var(_, _) | Expr::Int(_) | Expr::Bool(_) | Expr::Unit => true,
            Expr::BinOp { lhs, rhs, .. } => {
                Self::is_simple_base_case(lhs) && Self::is_simple_base_case(rhs)
            }
            Expr::UnaryOp { operand, .. } => Self::is_simple_base_case(operand),
            _ => false,
        }
    }

    fn try_speculate_call(
        &mut self,
        name: &str,
        ir_args: &[Expr],
    ) -> Result<Option<CValue>, String> {
        let func_idx = match self.func_names.get(name) {
            Some(&idx) => idx,
            None => return Ok(None),
        };
        if self.func_ids[func_idx].is_none() {
            return Ok(None);
        }
        let func = &self.ir_functions[func_idx];

        // Look for base case in then_branch OR else_branch.
        // base_in_then: true = base case is then_branch (branch to base when cond is truthy)
        //               false = base case is else_branch (branch to base when cond is falsy)
        let (cond_expr, base_expr, base_in_then) = match &func.body {
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                if Self::is_simple_base_case(then_branch) {
                    (condition.as_ref(), then_branch.as_ref(), true)
                } else if let Some(else_br) = else_branch
                    && Self::is_simple_base_case(else_br)
                {
                    (condition.as_ref(), else_br.as_ref(), false)
                } else {
                    return Ok(None);
                }
            }
            _ => return Ok(None),
        };

        if !expr_only_refs_params(cond_expr, &func.params) {
            return Ok(None);
        }
        if !expr_only_refs_params(base_expr, &func.params) {
            return Ok(None);
        }

        let arg_vals: Vec<CValue> = ir_args
            .iter()
            .map(|a| self.compile_expr(a))
            .collect::<Result<_, _>>()?;

        // Bind param vars so condition and base_expr can reference them
        let mut saved_vars = HashMap::new();
        for (i, param_name) in func.params.iter().enumerate() {
            let var = self.new_var();
            self.builder.def_var(var, arg_vals[i]);
            if let Some(old) = self.vars.insert(param_name.clone(), var) {
                saved_vars.insert(param_name.clone(), old);
            }
        }

        let cond_val = self.compile_expr(cond_expr)?;

        let base_block = self.builder.create_block();
        let call_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);

        let cmp = self.is_truthy(cond_val);
        if base_in_then {
            // Base case in then: branch to base when cond is true
            self.builder
                .ins()
                .brif(cmp, base_block, &[], call_block, &[]);
        } else {
            // Base case in else: branch to base when cond is false
            self.builder
                .ins()
                .brif(cmp, call_block, &[], base_block, &[]);
        }

        // base_block: compile base_expr only when the base case is taken
        self.builder.switch_to_block(base_block);
        self.builder.seal_block(base_block);
        let base_val = self.compile_expr(base_expr)?;
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(base_val)]);

        // call_block: emit the actual recursive call
        self.builder.switch_to_block(call_block);
        self.builder.seal_block(call_block);
        let func_id = self.func_ids[func_idx].ok_or_else(|| {
            format!(
                "JIT: speculated function at index {} was not declared",
                func_idx
            )
        })?;
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        let callee_unboxed = func_idx < self.unboxed_flags.len() && self.unboxed_flags[func_idx];
        let call_result = if callee_unboxed {
            // Callee expects raw i64: untag args, tag result
            let untagged_args: Vec<CValue> = arg_vals.iter().map(|&v| self.untag_int(v)).collect();
            let call = self.builder.ins().call(func_ref, &untagged_args);
            let raw = self.builder.inst_results(call)[0];
            self.tag_int_checked(raw)
        } else {
            let call = self.builder.ins().call(func_ref, &arg_vals);
            self.builder.inst_results(call)[0]
        };
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(call_result)]);

        // Restore original vars after both blocks are compiled
        for param_name in &func.params {
            self.vars.remove(param_name);
        }
        for (name, old_var) in saved_vars {
            self.vars.insert(name, old_var);
        }

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        Ok(Some(self.builder.block_params(merge_block)[0]))
    }

    // -- Unboxed expression compiler (scalar-only fast path) --
    //
    // Values are raw i64: ints as plain signed integers, bools as 0/1.
    // No NaN-boxing overhead. Boxing only happens at function boundaries.

    pub(super) fn compile_expr_unboxed(&mut self, expr: &Expr) -> Result<CValue, String> {
        match expr {
            Expr::Int(n) => Ok(self.builder.ins().iconst(types::I64, *n)),

            Expr::Float(f) => {
                // Float in unboxed mode: store as raw bits
                Ok(self.builder.ins().iconst(types::I64, f.to_bits() as i64))
            }

            Expr::Bool(b) => Ok(self.builder.ins().iconst(types::I64, *b as i64)),

            Expr::Unit => Ok(self.builder.ins().iconst(types::I64, 0)),

            Expr::Var(name, _) => {
                if let Some(&var) = self.vars.get(name) {
                    Ok(self.builder.use_var(var))
                } else {
                    Err(format!("Unknown variable: {}", name))
                }
            }

            Expr::BinOp { op, lhs, rhs, .. } => {
                let l = self.compile_expr_unboxed(lhs)?;
                let r = self.compile_expr_unboxed(rhs)?;
                match op {
                    BinOp::Add => Ok(self.builder.ins().iadd(l, r)),
                    BinOp::Sub => Ok(self.builder.ins().isub(l, r)),
                    BinOp::Mul => Ok(self.builder.ins().imul(l, r)),
                    BinOp::Div => Ok(self.builder.ins().sdiv(l, r)),
                    BinOp::Mod => Ok(self.builder.ins().srem(l, r)),
                    BinOp::Eq => {
                        let cmp = self.builder.ins().icmp(IntCC::Equal, l, r);
                        Ok(self.builder.ins().uextend(types::I64, cmp))
                    }
                    BinOp::Ne => {
                        let cmp = self.builder.ins().icmp(IntCC::NotEqual, l, r);
                        Ok(self.builder.ins().uextend(types::I64, cmp))
                    }
                    BinOp::Lt => {
                        let cmp = self.builder.ins().icmp(IntCC::SignedLessThan, l, r);
                        Ok(self.builder.ins().uextend(types::I64, cmp))
                    }
                    BinOp::Gt => {
                        let cmp = self.builder.ins().icmp(IntCC::SignedGreaterThan, l, r);
                        Ok(self.builder.ins().uextend(types::I64, cmp))
                    }
                    BinOp::Le => {
                        let cmp = self.builder.ins().icmp(IntCC::SignedLessThanOrEqual, l, r);
                        Ok(self.builder.ins().uextend(types::I64, cmp))
                    }
                    BinOp::Ge => {
                        let cmp = self
                            .builder
                            .ins()
                            .icmp(IntCC::SignedGreaterThanOrEqual, l, r);
                        Ok(self.builder.ins().uextend(types::I64, cmp))
                    }
                    BinOp::ListConcat => Err("ListConcat cannot be JIT-compiled".into()),
                }
            }

            Expr::UnaryOp { op, operand, .. } => {
                let val = self.compile_expr_unboxed(operand)?;
                match op {
                    UnaryOp::Neg => Ok(self.builder.ins().ineg(val)),
                    UnaryOp::Not => {
                        let zero = self.builder.ins().iconst(types::I64, 0);
                        let cmp = self.builder.ins().icmp(IntCC::Equal, val, zero);
                        Ok(self.builder.ins().uextend(types::I64, cmp))
                    }
                }
            }

            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let cond_val = self.compile_expr_unboxed(condition)?;

                let then_block = self.builder.create_block();
                let else_block = self.builder.create_block();
                let merge_block = self.builder.create_block();
                self.builder.append_block_param(merge_block, types::I64);

                // In unboxed mode, truthy = != 0
                let zero = self.builder.ins().iconst(types::I64, 0);
                let cmp = self.builder.ins().icmp(IntCC::NotEqual, cond_val, zero);
                self.builder
                    .ins()
                    .brif(cmp, then_block, &[], else_block, &[]);

                self.builder.switch_to_block(then_block);
                self.builder.seal_block(then_block);
                let then_val = self.compile_expr_unboxed(then_branch)?;
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(then_val)]);

                self.builder.switch_to_block(else_block);
                self.builder.seal_block(else_block);
                let else_val = if let Some(else_expr) = else_branch {
                    self.compile_expr_unboxed(else_expr)?
                } else {
                    self.builder.ins().iconst(types::I64, 0)
                };
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(else_val)]);

                self.builder.switch_to_block(merge_block);
                self.builder.seal_block(merge_block);
                Ok(self.builder.block_params(merge_block)[0])
            }

            Expr::Let { pattern, value, .. } => {
                let val = self.compile_expr_unboxed(value)?;
                // Only Var patterns in scalar-only functions
                if let Pattern::Var(name) = pattern.as_ref() {
                    let var = self.new_var();
                    self.builder.def_var(var, val);
                    self.vars.insert(name.clone(), var);
                }
                Ok(self.builder.ins().iconst(types::I64, 0))
            }

            Expr::Assign { name, value } => {
                let val = self.compile_expr_unboxed(value)?;
                if let Some(&var) = self.vars.get(name) {
                    self.builder.def_var(var, val);
                }
                Ok(self.builder.ins().iconst(types::I64, 0))
            }

            Expr::FieldAssign { object, field, value } => {
                // SRA fast path
                if let Some(field_vars) = self.sra_records.get(object.as_str()) {
                    if let Some(&fvar) = field_vars.get(field.as_str()) {
                        let val = self.compile_expr_unboxed(value)?;
                        self.builder.def_var(fvar, val);
                        return Ok(self.builder.ins().iconst(types::I64, 0));
                    }
                }
                // Fall back to boxed path
                self.compile_expr(&Expr::FieldAssign {
                    object: object.clone(),
                    field: field.clone(),
                    value: value.clone(),
                })
            }

            Expr::Block(exprs, _) => {
                let mut result = self.builder.ins().iconst(types::I64, 0);
                for e in exprs {
                    result = self.compile_expr_unboxed(e)?;
                }
                Ok(result)
            }

            Expr::CallDirect { name, args, .. } => {
                // Try base-case speculation first (unboxed mode)
                if let Some(val) = self.try_speculate_call_unboxed(name, args)? {
                    return Ok(val);
                }

                if let Some(&func_idx) = self.func_names.get(name) {
                    if let Some(func_id) = self.func_ids[func_idx] {
                        let callee_unboxed =
                            func_idx < self.unboxed_flags.len() && self.unboxed_flags[func_idx];
                        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);

                        if callee_unboxed {
                            // Unboxed → unboxed: pass raw values, receive raw result
                            let arg_vals: Vec<CValue> = args
                                .iter()
                                .map(|a| self.compile_expr_unboxed(a))
                                .collect::<Result<Vec<_>, _>>()?;
                            let call = self.builder.ins().call(func_ref, &arg_vals);
                            Ok(self.builder.inst_results(call)[0])
                        } else {
                            // Unboxed → boxed: tag args, untag result
                            let arg_vals: Vec<CValue> = args
                                .iter()
                                .map(|a| {
                                    let raw = self.compile_expr_unboxed(a)?;
                                    Ok(self.tag_int_checked(raw))
                                })
                                .collect::<Result<Vec<_>, String>>()?;
                            let call = self.builder.ins().call(func_ref, &arg_vals);
                            let boxed_result = self.builder.inst_results(call)[0];
                            Ok(self.untag_int(boxed_result))
                        }
                    } else {
                        Err(format!(
                            "Function '{}' not JIT-compiled (fallback required)",
                            name
                        ))
                    }
                } else {
                    Err(format!("Unknown function: {}", name))
                }
            }

            Expr::TailCall { name, args, .. } => {
                if name == &self.current_func_name
                    && let Some(loop_header) = self.loop_header
                {
                    // Self-recursive tail call in unboxed mode: pass raw values
                    let arg_vals: Vec<CValue> = args
                        .iter()
                        .map(|a| self.compile_expr_unboxed(a))
                        .collect::<Result<Vec<_>, _>>()?;

                    for (var, val) in self.param_vars.iter().zip(arg_vals.iter()) {
                        self.builder.def_var(*var, *val);
                    }

                    self.builder.ins().jump(loop_header, &[]);

                    let dead_block = self.builder.create_block();
                    self.builder.switch_to_block(dead_block);
                    self.builder.seal_block(dead_block);

                    return Ok(self.builder.ins().iconst(types::I64, 0));
                }

                // Non-self tail call → regular call
                if let Some(&func_idx) = self.func_names.get(name) {
                    if let Some(func_id) = self.func_ids[func_idx] {
                        let callee_unboxed =
                            func_idx < self.unboxed_flags.len() && self.unboxed_flags[func_idx];
                        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);

                        if callee_unboxed {
                            // Both unboxed — use return_call for guaranteed TCE
                            let arg_vals: Vec<CValue> = args
                                .iter()
                                .map(|a| self.compile_expr_unboxed(a))
                                .collect::<Result<Vec<_>, _>>()?;
                            self.builder.ins().return_call(func_ref, &arg_vals);

                            let dead_block = self.builder.create_block();
                            self.builder.switch_to_block(dead_block);
                            self.builder.seal_block(dead_block);
                            Ok(self.builder.ins().iconst(types::I64, 0))
                        } else {
                            let arg_vals: Vec<CValue> = args
                                .iter()
                                .map(|a| {
                                    let raw = self.compile_expr_unboxed(a)?;
                                    Ok(self.tag_int_checked(raw))
                                })
                                .collect::<Result<Vec<_>, String>>()?;
                            let call = self.builder.ins().call(func_ref, &arg_vals);
                            let boxed_result = self.builder.inst_results(call)[0];
                            Ok(self.untag_int(boxed_result))
                        }
                    } else {
                        Err(format!(
                            "Function '{}' not JIT-compiled (fallback required)",
                            name
                        ))
                    }
                } else {
                    Err(format!("Unknown function: {}", name))
                }
            }

            Expr::And(a, b) => {
                let a_val = self.compile_expr_unboxed(a)?;
                let zero_val = self.builder.ins().iconst(types::I64, 0);

                let eval_b = self.builder.create_block();
                let merge = self.builder.create_block();
                self.builder.append_block_param(merge, types::I64);

                let cmp = self.builder.ins().icmp(IntCC::NotEqual, a_val, zero_val);
                self.builder
                    .ins()
                    .brif(cmp, eval_b, &[], merge, &[BlockArg::Value(zero_val)]);

                self.builder.switch_to_block(eval_b);
                self.builder.seal_block(eval_b);
                let b_val = self.compile_expr_unboxed(b)?;
                self.builder.ins().jump(merge, &[BlockArg::Value(b_val)]);

                self.builder.switch_to_block(merge);
                self.builder.seal_block(merge);
                Ok(self.builder.block_params(merge)[0])
            }

            Expr::Or(a, b) => {
                let a_val = self.compile_expr_unboxed(a)?;
                let one_val = self.builder.ins().iconst(types::I64, 1);

                let eval_b = self.builder.create_block();
                let merge = self.builder.create_block();
                self.builder.append_block_param(merge, types::I64);

                let zero = self.builder.ins().iconst(types::I64, 0);
                let cmp = self.builder.ins().icmp(IntCC::NotEqual, a_val, zero);
                self.builder
                    .ins()
                    .brif(cmp, merge, &[BlockArg::Value(one_val)], eval_b, &[]);

                self.builder.switch_to_block(eval_b);
                self.builder.seal_block(eval_b);
                let b_val = self.compile_expr_unboxed(b)?;
                self.builder.ins().jump(merge, &[BlockArg::Value(b_val)]);

                self.builder.switch_to_block(merge);
                self.builder.seal_block(merge);
                Ok(self.builder.block_params(merge)[0])
            }

            _ => Err(format!("Unsupported expression in unboxed JIT: {:?}", expr)),
        }
    }

    /// Base-case speculation for unboxed mode.
    /// Same logic as try_speculate_call but using raw unboxed values.
    fn try_speculate_call_unboxed(
        &mut self,
        name: &str,
        ir_args: &[Expr],
    ) -> Result<Option<CValue>, String> {
        let func_idx = match self.func_names.get(name) {
            Some(&idx) => idx,
            None => return Ok(None),
        };
        if self.func_ids[func_idx].is_none() {
            return Ok(None);
        }
        // Only speculate on unboxed callees
        if func_idx >= self.unboxed_flags.len() || !self.unboxed_flags[func_idx] {
            return Ok(None);
        }
        let func = &self.ir_functions[func_idx];

        // Look for base case in then_branch OR else_branch
        let (cond_expr, base_expr, base_in_then) = match &func.body {
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                if Self::is_simple_base_case(then_branch) {
                    (condition.as_ref(), then_branch.as_ref(), true)
                } else if let Some(else_br) = else_branch
                    && Self::is_simple_base_case(else_br)
                {
                    (condition.as_ref(), else_br.as_ref(), false)
                } else {
                    return Ok(None);
                }
            }
            _ => return Ok(None),
        };

        if !expr_only_refs_params(cond_expr, &func.params) {
            return Ok(None);
        }
        if !expr_only_refs_params(base_expr, &func.params) {
            return Ok(None);
        }

        // Compile args in unboxed mode
        let arg_vals: Vec<CValue> = ir_args
            .iter()
            .map(|a| self.compile_expr_unboxed(a))
            .collect::<Result<_, _>>()?;

        // Temporarily bind callee params to arg values
        let mut saved_vars = HashMap::new();
        for (i, param_name) in func.params.iter().enumerate() {
            let var = self.new_var();
            self.builder.def_var(var, arg_vals[i]);
            if let Some(old) = self.vars.insert(param_name.clone(), var) {
                saved_vars.insert(param_name.clone(), old);
            }
        }

        // Compile condition and base case in unboxed mode
        let cond_val = self.compile_expr_unboxed(cond_expr)?;
        let base_val = self.compile_expr_unboxed(base_expr)?;

        // Restore vars
        for param_name in &func.params {
            self.vars.remove(param_name);
        }
        for (name, old_var) in saved_vars {
            self.vars.insert(name, old_var);
        }

        let call_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);

        // Unboxed truthy check: != 0
        let zero = self.builder.ins().iconst(types::I64, 0);
        let cmp = self.builder.ins().icmp(IntCC::NotEqual, cond_val, zero);
        if base_in_then {
            self.builder.ins().brif(
                cmp,
                merge_block,
                &[BlockArg::Value(base_val)],
                call_block,
                &[],
            );
        } else {
            self.builder.ins().brif(
                cmp,
                call_block,
                &[],
                merge_block,
                &[BlockArg::Value(base_val)],
            );
        }

        self.builder.switch_to_block(call_block);
        self.builder.seal_block(call_block);

        // Call the unboxed function: pass raw args, receive raw result
        let func_id = self.func_ids[func_idx].ok_or_else(|| {
            format!(
                "JIT: unboxed speculated function at index {} was not declared",
                func_idx
            )
        })?;
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        let call = self.builder.ins().call(func_ref, &arg_vals);
        let call_result = self.builder.inst_results(call)[0];
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(call_result)]);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        Ok(Some(self.builder.block_params(merge_block)[0]))
    }

    // -- Main expression compiler --

    pub(super) fn compile_expr(&mut self, expr: &Expr) -> Result<CValue, String> {
        match expr {
            Expr::Int(n) => Ok(self.emit_nvalue(NValue::int(*n))),

            Expr::Float(f) => Ok(self.emit_nvalue(NValue::float(*f))),

            Expr::Bool(b) => {
                let bits = if *b { NV_TRUE } else { NV_FALSE };
                Ok(self.builder.ins().iconst(types::I64, bits as i64))
            }

            Expr::Unit => Ok(self.builder.ins().iconst(types::I64, NV_UNIT as i64)),

            Expr::String(s) => self.emit_string_value(s),

            Expr::Var(name, _) => {
                if let Some(&var) = self.vars.get(name) {
                    let val = self.builder.use_var(var);
                    // RC: incref on read — scope still owns the original
                    if self.rc_enabled {
                        Ok(self.emit_incref(val))
                    } else {
                        Ok(val)
                    }
                } else if let Some(field_vars) = self.sra_records.get(name).cloned() {
                    // SRA materialization: rebuild a boxed record from scalar fields
                    let mut sorted_fields: Vec<_> = field_vars.iter().collect();
                    sorted_fields.sort_by(|a, b| a.0.cmp(b.0));
                    let mut pair_vals = Vec::with_capacity(sorted_fields.len() * 2);
                    for &(ref fname, &fvar) in &sorted_fields {
                        let key_val = self.emit_string_value(fname)?;
                        let key_val = self.stash_in_var(key_val);
                        let val = self.builder.use_var(fvar);
                        pair_vals.push(key_val);
                        pair_vals.push(val);
                    }
                    let addr = self.spill_to_stack(&pair_vals);
                    let count = self
                        .builder
                        .ins()
                        .iconst(types::I64, sorted_fields.len() as i64);
                    Ok(self.call_helper("jit_make_record", &[addr, count]))
                } else if let Some(&func_idx) = self.func_names.get(name) {
                    // Function reference as NaN-boxed Function value
                    Ok(self.emit_nvalue(NValue::function(func_idx)))
                } else {
                    Err(format!("Unknown variable: {}", name))
                }
            }

            Expr::BinOp { op, lhs, rhs, ty } => {
                let is_int = matches!(ty, Some(Type::Int));
                let is_float = matches!(ty, Some(Type::Float));
                let lhs_val = self.compile_expr(lhs)?;
                let lhs_val = self.stash_in_var(lhs_val);
                let rhs_val = self.compile_expr(rhs)?;

                if is_int {
                    // Use runtime helpers for arithmetic ops to correctly handle
                    // BigInt overflow (values exceeding 48-bit inline range).
                    // Comparisons that don't produce overflow can use inline ops
                    // when both inputs are guaranteed to be inline ints.
                    match op {
                        BinOp::Add => Ok(self.call_helper("jit_int_add", &[lhs_val, rhs_val])),
                        BinOp::Sub => Ok(self.call_helper("jit_int_sub", &[lhs_val, rhs_val])),
                        BinOp::Mul => Ok(self.call_helper("jit_int_mul", &[lhs_val, rhs_val])),
                        BinOp::Div => Ok(self.call_helper("jit_int_div", &[lhs_val, rhs_val])),
                        BinOp::Mod => Ok(self.call_helper("jit_int_mod", &[lhs_val, rhs_val])),
                        BinOp::Eq => Ok(self.call_helper("jit_values_equal", &[lhs_val, rhs_val])),
                        BinOp::Ne => {
                            let eq = self.call_helper("jit_values_equal", &[lhs_val, rhs_val]);
                            let one = self.builder.ins().iconst(types::I64, 1);
                            Ok(self.builder.ins().bxor(eq, one))
                        }
                        BinOp::Lt => Ok(self.call_helper("jit_int_lt", &[lhs_val, rhs_val])),
                        BinOp::Gt => Ok(self.call_helper("jit_int_gt", &[lhs_val, rhs_val])),
                        BinOp::Le => Ok(self.call_helper("jit_int_le", &[lhs_val, rhs_val])),
                        BinOp::Ge => Ok(self.call_helper("jit_int_ge", &[lhs_val, rhs_val])),
                        BinOp::ListConcat => {
                            Ok(self.call_helper("jit_list_concat", &[lhs_val, rhs_val]))
                        }
                    }
                } else if is_float {
                    // Native Cranelift F64 instructions via bitcast I64↔F64.
                    // NaN-boxed floats ARE raw IEEE 754 bits, so bitcast is zero-overhead.
                    let lf = self.bitcast_to_f64(lhs_val);
                    let rf = self.bitcast_to_f64(rhs_val);
                    match op {
                        BinOp::Add => {
                            let r = self.builder.ins().fadd(lf, rf);
                            Ok(self.bitcast_to_i64(r))
                        }
                        BinOp::Sub => {
                            let r = self.builder.ins().fsub(lf, rf);
                            Ok(self.bitcast_to_i64(r))
                        }
                        BinOp::Mul => {
                            let r = self.builder.ins().fmul(lf, rf);
                            Ok(self.bitcast_to_i64(r))
                        }
                        BinOp::Div => {
                            let r = self.builder.ins().fdiv(lf, rf);
                            Ok(self.bitcast_to_i64(r))
                        }
                        BinOp::Mod => {
                            // No Cranelift fmod instruction; use runtime helper.
                            Ok(self.call_helper("jit_float_mod", &[lhs_val, rhs_val]))
                        }
                        BinOp::Eq => {
                            let c = self.builder.ins().fcmp(FloatCC::Equal, lf, rf);
                            Ok(self.tag_bool(c))
                        }
                        BinOp::Ne => {
                            let c = self.builder.ins().fcmp(FloatCC::NotEqual, lf, rf);
                            Ok(self.tag_bool(c))
                        }
                        BinOp::Lt => {
                            let c = self.builder.ins().fcmp(FloatCC::LessThan, lf, rf);
                            Ok(self.tag_bool(c))
                        }
                        BinOp::Gt => {
                            let c = self.builder.ins().fcmp(FloatCC::GreaterThan, lf, rf);
                            Ok(self.tag_bool(c))
                        }
                        BinOp::Le => {
                            let c = self.builder.ins().fcmp(FloatCC::LessThanOrEqual, lf, rf);
                            Ok(self.tag_bool(c))
                        }
                        BinOp::Ge => {
                            let c = self.builder.ins().fcmp(FloatCC::GreaterThanOrEqual, lf, rf);
                            Ok(self.tag_bool(c))
                        }
                        BinOp::ListConcat => Err("ListConcat on Float is invalid".into()),
                    }
                } else {
                    // Non-int: compare raw NaN-boxed bits for equality/ordering,
                    // or use runtime helpers for complex types.
                    // For simple cases (Bool comparison), raw bit comparison works.
                    match op {
                        BinOp::Eq => {
                            let result = self.call_helper("jit_values_equal", &[lhs_val, rhs_val]);
                            Ok(result)
                        }
                        BinOp::Ne => {
                            let eq = self.call_helper("jit_values_equal", &[lhs_val, rhs_val]);
                            // Flip: if eq is NV_TRUE, return NV_FALSE and vice versa
                            let one = self.builder.ins().iconst(types::I64, 1);
                            Ok(self.builder.ins().bxor(eq, one))
                        }
                        // For non-int arithmetic, use runtime helpers (handles BigInt)
                        BinOp::Add => Ok(self.call_helper("jit_int_add", &[lhs_val, rhs_val])),
                        BinOp::Sub => Ok(self.call_helper("jit_int_sub", &[lhs_val, rhs_val])),
                        BinOp::Mul => Ok(self.call_helper("jit_int_mul", &[lhs_val, rhs_val])),
                        BinOp::Div => Ok(self.call_helper("jit_int_div", &[lhs_val, rhs_val])),
                        BinOp::Mod => Ok(self.call_helper("jit_int_mod", &[lhs_val, rhs_val])),
                        // For ordering comparisons without type annotation, use inline
                        // NaN-boxed comparison (works correctly for inline ints; for BigInt
                        // the type checker should provide ty=Int so the is_int path is used).
                        BinOp::Lt => {
                            let a = self.untag_int(lhs_val);
                            let b = self.untag_int(rhs_val);
                            let cmp = self.builder.ins().icmp(IntCC::SignedLessThan, a, b);
                            Ok(self.tag_bool(cmp))
                        }
                        BinOp::Gt => {
                            let a = self.untag_int(lhs_val);
                            let b = self.untag_int(rhs_val);
                            let cmp = self.builder.ins().icmp(IntCC::SignedGreaterThan, a, b);
                            Ok(self.tag_bool(cmp))
                        }
                        BinOp::Le => {
                            let a = self.untag_int(lhs_val);
                            let b = self.untag_int(rhs_val);
                            let cmp = self.builder.ins().icmp(IntCC::SignedLessThanOrEqual, a, b);
                            Ok(self.tag_bool(cmp))
                        }
                        BinOp::Ge => {
                            let a = self.untag_int(lhs_val);
                            let b = self.untag_int(rhs_val);
                            let cmp =
                                self.builder
                                    .ins()
                                    .icmp(IntCC::SignedGreaterThanOrEqual, a, b);
                            Ok(self.tag_bool(cmp))
                        }
                        BinOp::ListConcat => {
                            if matches!(ty, Some(Type::String)) {
                                Ok(self.call_helper("jit_string_concat", &[lhs_val, rhs_val]))
                            } else {
                                Ok(self.call_helper("jit_list_concat", &[lhs_val, rhs_val]))
                            }
                        }
                    }
                }
            }

            Expr::UnaryOp { op, operand, ty } => {
                let val = self.compile_expr(operand)?;
                match op {
                    UnaryOp::Neg => {
                        if matches!(ty, Some(Type::Float)) {
                            let f = self.bitcast_to_f64(val);
                            let neg = self.builder.ins().fneg(f);
                            Ok(self.bitcast_to_i64(neg))
                        } else {
                            Ok(self.call_helper("jit_int_neg", &[val]))
                        }
                    }
                    UnaryOp::Not => {
                        // Check bit 0 (truthy check), invert
                        let one = self.builder.ins().iconst(types::I64, 1);
                        let bit = self.builder.ins().band(val, one);
                        let zero = self.builder.ins().iconst(types::I64, 0);
                        let is_falsy = self.builder.ins().icmp(IntCC::Equal, bit, zero);
                        Ok(self.tag_bool(is_falsy))
                    }
                }
            }

            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let cond_val = self.compile_expr(condition)?;

                let then_block = self.builder.create_block();
                let else_block = self.builder.create_block();
                let merge_block = self.builder.create_block();
                self.builder.append_block_param(merge_block, types::I64);

                let cmp = self.is_truthy(cond_val);
                // RC: is_truthy borrows cond_val; decref the owned value now
                if self.rc_enabled {
                    self.emit_decref(cond_val);
                }
                self.builder
                    .ins()
                    .brif(cmp, then_block, &[], else_block, &[]);

                self.builder.switch_to_block(then_block);
                self.builder.seal_block(then_block);
                let then_val = self.compile_expr(then_branch)?;
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(then_val)]);

                self.builder.switch_to_block(else_block);
                self.builder.seal_block(else_block);
                let else_val = if let Some(else_expr) = else_branch {
                    self.compile_expr(else_expr)?
                } else {
                    self.builder.ins().iconst(types::I64, NV_UNIT as i64)
                };
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(else_val)]);

                self.builder.switch_to_block(merge_block);
                self.builder.seal_block(merge_block);
                Ok(self.builder.block_params(merge_block)[0])
            }

            Expr::Let { pattern, value, .. } => {
                // Optimization: self-rebinding UpdateRecord (let x = { ..x, f: v })
                // Skip incref on base for in-place mutation when refcount == 1.
                if self.rc_enabled
                    && let Pattern::Var(bind_name) = pattern.as_ref()
                    && let Expr::UpdateRecord { base, .. } = value.as_ref()
                    && let Expr::Var(base_name, _) = base.as_ref()
                    && bind_name == base_name
                    && self.vars.contains_key(bind_name)
                {
                    let val =
                        self.compile_update_record_consuming(value, bind_name)?;
                    self.bind_pattern_rc(pattern, val)?;
                } else {
                    let val = self.compile_expr(value)?;
                    self.bind_pattern_rc(pattern, val)?;
                }
                self.alias_sra_pattern_bindings(pattern, value);
                if let Some(value_ty) = Self::expr_type_hint(value) {
                    self.reseed_hot_pattern_vars(pattern, &value_ty)?;
                }
                Ok(self.builder.ins().iconst(types::I64, NV_UNIT as i64))
            }

            Expr::Assign { name, value } => {
                let val = self.compile_expr(value)?;
                if let Some(&var) = self.vars.get(name) {
                    // RC: decref the old value before overwriting
                    if self.rc_enabled {
                        let old = self.builder.use_var(var);
                        self.emit_decref(old);
                    }
                    self.builder.def_var(var, val);
                }
                Ok(self.builder.ins().iconst(types::I64, NV_UNIT as i64))
            }

            Expr::FieldAssign { object, field, value } => {
                let val = self.compile_expr(value)?;
                // SRA fast path: if object is scalar-replaced, just update the field variable
                if let Some(field_vars) = self.sra_records.get(object.as_str()) {
                    if let Some(&fvar) = field_vars.get(field.as_str()) {
                        // RC: decref old field value before overwriting
                        if self.rc_enabled {
                            let old = self.builder.use_var(fvar);
                            self.emit_decref(old);
                        }
                        self.builder.def_var(fvar, val);
                        return Ok(self.builder.ins().iconst(types::I64, NV_UNIT as i64));
                    }
                }
                // Boxed path: create updated record and rebind the object variable
                if let Some(&obj_var) = self.vars.get(object.as_str()) {
                    let obj_val = self.builder.use_var(obj_var);
                    // Resolve field index if type info available
                    let obj_ty = Self::expr_type_hint(&Expr::Var(object.clone(), None));
                    let field_idx = obj_ty
                        .as_ref()
                        .and_then(|t| Self::sorted_field_index(t, field));
                    let updated = if let Some(idx) = field_idx {
                        let idx_val = self.builder.ins().iconst(types::I64, idx as i64);
                        self.call_helper("jit_update_record_indexed1", &[obj_val, idx_val, val])
                    } else {
                        // Fall back to string-based update via general helper
                        let idx_val = self.builder.ins().iconst(types::I64, 0);
                        self.call_helper("jit_update_record", &[obj_val, idx_val, val])
                    };
                    if self.rc_enabled {
                        let old = self.builder.use_var(obj_var);
                        self.emit_decref(old);
                    }
                    self.builder.def_var(obj_var, updated);
                }
                Ok(self.builder.ins().iconst(types::I64, NV_UNIT as i64))
            }

            Expr::Block(exprs, _) => {
                // SRA: find non-escaping record bindings and track them
                let self_fn = if self.loop_header.is_some() { Some(self.current_func_name.as_str()) } else { None };
                let sra_candidates = Self::find_sra_candidates_with_existing(exprs, &self.sra_records, self_fn);
                let sra_names: std::collections::HashSet<&str> =
                    sra_candidates.iter().map(|(n, _)| n.as_str()).collect();

                self.push_rc_scope();
                let mut result = self.builder.ins().iconst(types::I64, NV_UNIT as i64);
                for (idx, e) in exprs.iter().enumerate() {
                    let is_last = idx == exprs.len() - 1;
                    // SRA: intercept Let bindings to MakeRecord for candidates
                    if self.try_compile_sra_let(e, &sra_names)? {
                        result = self.builder.ins().iconst(types::I64, NV_UNIT as i64);
                        continue;
                    }
                    result = self.compile_expr(e)?;
                    // RC: decref intermediate results (not the last expression)
                    if self.rc_enabled && !is_last && !matches!(e, Expr::Let { .. } | Expr::Assign { .. } | Expr::FieldAssign { .. }) {
                        self.emit_decref(result);
                    }
                }
                // RC: store result in a variable so pop_rc_scope can exclude it
                if self.rc_enabled {
                    let result_var = self.new_var();
                    self.builder.def_var(result_var, result);
                    self.pop_rc_scope(Some(result_var));
                    result = self.builder.use_var(result_var);
                } else {
                    self.pop_rc_scope(None);
                }
                Ok(result)
            }

            Expr::CallDirect { name, args, .. } => {
                if let Some(val) = self.try_speculate_call(name, args)? {
                    return Ok(val);
                }

                if let Some(&func_idx) = self.func_names.get(name) {
                    if let Some(func_id) = self.func_ids[func_idx] {
                        let callee_unboxed =
                            func_idx < self.unboxed_flags.len() && self.unboxed_flags[func_idx];
                        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);

                        if callee_unboxed {
                            // Boxed → unboxed: untag args, tag result
                            let arg_vals = self.compile_exprs_stashed(args)?;
                            let arg_vals: Vec<CValue> = arg_vals
                                .into_iter()
                                .map(|boxed| self.untag_int(boxed))
                                .collect();
                            let call = self.builder.ins().call(func_ref, &arg_vals);
                            let raw_result = self.builder.inst_results(call)[0];
                            Ok(self.tag_int_checked(raw_result))
                        } else if let Some(ref field_names) = self.multireturn_info.get(func_idx).and_then(|x| x.as_ref()) {
                            // Multi-return callee: receive N values, reconstruct boxed record
                            let arg_vals = self.compile_exprs_stashed(args)?;
                            let call = self.builder.ins().call(func_ref, &arg_vals);
                            let results = self.builder.inst_results(call);
                            let field_vals: Vec<CValue> = results.to_vec();

                            // Build a boxed record from the individual field values.
                            let mut pair_vals = Vec::with_capacity(field_names.len() * 2);
                            for (idx, fname) in field_names.iter().enumerate() {
                                let name_val = self.emit_string_value(fname)?;
                                pair_vals.push(name_val);
                                pair_vals.push(field_vals[idx]);
                            }
                            let addr = self.spill_to_stack(&pair_vals);
                            let count = self.builder.ins().iconst(types::I64, field_names.len() as i64);
                            Ok(self.call_helper("jit_make_record", &[addr, count]))
                        } else {
                            let arg_vals = self.compile_exprs_stashed(args)?;
                            let call = self.builder.ins().call(func_ref, &arg_vals);
                            Ok(self.builder.inst_results(call)[0])
                        }
                    } else {
                        Err(format!(
                            "Function '{}' not JIT-compiled (fallback required)",
                            name
                        ))
                    }
                } else {
                    Err(format!("Unknown function: {}", name))
                }
            }

            Expr::TailCall { name, args, .. } => {
                if name == &self.current_func_name
                    && let Some(loop_header) = self.loop_header
                {
                    // SRA-aware tail call: for params that are SRA'd, directly
                    // rebind the param SRA field vars instead of materializing
                    // boxed records. This eliminates allocation in hot loops.
                    if !self.param_sra_original.is_empty() {
                        self.compile_sra_tail_call(args, loop_header)?;
                        return Ok(self.builder.ins().iconst(types::I64, NV_UNIT as i64));
                    }

                    let arg_vals = self.compile_exprs_stashed(args)?;

                    // RC: decref old parameter values before overwriting
                    if self.rc_enabled {
                        let pvars: Vec<Variable> = self.param_vars.clone();
                        for pv in &pvars {
                            let old_val = self.builder.use_var(*pv);
                            self.emit_decref(old_val);
                        }
                    }

                    for (var, val) in self.param_vars.iter().zip(arg_vals.iter()) {
                        self.builder.def_var(*var, *val);
                    }

                    self.builder.ins().jump(loop_header, &[]);

                    let dead_block = self.builder.create_block();
                    self.builder.switch_to_block(dead_block);
                    self.builder.seal_block(dead_block);

                    return Ok(self.builder.ins().iconst(types::I64, NV_UNIT as i64));
                }

                // Non-self tail call → regular call
                if let Some(&func_idx) = self.func_names.get(name) {
                    if let Some(func_id) = self.func_ids[func_idx] {
                        let callee_unboxed =
                            func_idx < self.unboxed_flags.len() && self.unboxed_flags[func_idx];
                        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);

                        if callee_unboxed {
                            let arg_vals = self.compile_exprs_stashed(args)?;
                            let arg_vals: Vec<CValue> = arg_vals
                                .into_iter()
                                .map(|boxed| self.untag_int(boxed))
                                .collect();
                            let call = self.builder.ins().call(func_ref, &arg_vals);
                            let raw_result = self.builder.inst_results(call)[0];
                            Ok(self.tag_int_checked(raw_result))
                        } else {
                            // Both boxed — use return_call for guaranteed TCE
                            let arg_vals = self.compile_exprs_stashed(args)?;
                            self.builder.ins().return_call(func_ref, &arg_vals);

                            let dead_block = self.builder.create_block();
                            self.builder.switch_to_block(dead_block);
                            self.builder.seal_block(dead_block);
                            Ok(self.builder.ins().iconst(types::I64, NV_UNIT as i64))
                        }
                    } else {
                        Err(format!(
                            "Function '{}' not JIT-compiled (fallback required)",
                            name
                        ))
                    }
                } else {
                    Err(format!("Unknown function: {}", name))
                }
            }

            Expr::And(a, b) => {
                let a_val = self.compile_expr(a)?;
                let false_val = self.builder.ins().iconst(types::I64, NV_FALSE as i64);
                let cmp = self.is_truthy(a_val);
                // RC: is_truthy borrows a_val; decref the owned value now
                if self.rc_enabled {
                    self.emit_decref(a_val);
                }

                let eval_b = self.builder.create_block();
                let merge = self.builder.create_block();
                self.builder.append_block_param(merge, types::I64);

                self.builder
                    .ins()
                    .brif(cmp, eval_b, &[], merge, &[BlockArg::Value(false_val)]);

                self.builder.switch_to_block(eval_b);
                self.builder.seal_block(eval_b);
                let b_val = self.compile_expr(b)?;
                self.builder.ins().jump(merge, &[BlockArg::Value(b_val)]);

                self.builder.switch_to_block(merge);
                self.builder.seal_block(merge);
                Ok(self.builder.block_params(merge)[0])
            }

            Expr::Or(a, b) => {
                let a_val = self.compile_expr(a)?;
                let true_val = self.builder.ins().iconst(types::I64, NV_TRUE as i64);
                let cmp = self.is_truthy(a_val);
                // RC: is_truthy borrows a_val; decref the owned value now
                if self.rc_enabled {
                    self.emit_decref(a_val);
                }

                let eval_b = self.builder.create_block();
                let merge = self.builder.create_block();
                self.builder.append_block_param(merge, types::I64);

                self.builder
                    .ins()
                    .brif(cmp, merge, &[BlockArg::Value(true_val)], eval_b, &[]);

                self.builder.switch_to_block(eval_b);
                self.builder.seal_block(eval_b);
                let b_val = self.compile_expr(b)?;
                self.builder.ins().jump(merge, &[BlockArg::Value(b_val)]);

                self.builder.switch_to_block(merge);
                self.builder.seal_block(merge);
                Ok(self.builder.block_params(merge)[0])
            }

            // -- Phase 2: CallNative + Concat --
            Expr::CallNative {
                module: mod_name,
                method,
                args,
                ty,
            } => {
                let qualified = if mod_name.is_empty() {
                    method.to_string()
                } else {
                    format!("{}.{}", mod_name, method)
                };

                // Direct JIT helper for String.reverse (bypass native dispatch)
                if qualified == "String.reverse" && args.len() == 1 {
                    let arg = self.compile_expr(&args[0])?;
                    return Ok(self.call_helper("jit_string_reverse", &[arg]));
                }

                // Direct Cranelift intrinsic for Math.sqrt on statically-float args.
                // Keep fallback native dispatch for non-float/unknown numeric shapes
                // (e.g. Int/BigInt coercions handled by native runtime path).
                if qualified == "Math.sqrt"
                    && args.len() == 1
                    && matches!(ty, Some(Type::Float))
                    && Self::expr_is_known_float(&args[0])
                {
                    let arg = self.compile_expr(&args[0])?;
                    let arg_f = self.bitcast_to_f64(arg);
                    let out_f = self.builder.ins().sqrt(arg_f);
                    return Ok(self.bitcast_to_i64(out_f));
                }

                // AOT path: direct call to extern "C" symbol in libbaseline_rt
                if let Some(native_ids) = self.aot_native_ids {
                    let func_id = native_ids
                        .get(&qualified)
                        .ok_or_else(|| format!("AOT: unsupported native: {}", qualified))?;

                    let arg_vals: Vec<CValue> = args
                        .iter()
                        .map(|a| self.compile_expr(a))
                        .collect::<Result<Vec<_>, _>>()?;

                    let args_addr = self.spill_to_stack(&arg_vals);
                    let count_val = self.builder.ins().iconst(types::I64, arg_vals.len() as i64);

                    let func_ref = self
                        .module
                        .declare_func_in_func(*func_id, self.builder.func);
                    let call = self.builder.ins().call(func_ref, &[args_addr, count_val]);
                    return Ok(self.builder.inst_results(call)[0]);
                }

                // JIT path: dispatch through NativeRegistry
                let registry = self.natives.ok_or("No native registry for JIT")?;
                let native_id = registry
                    .lookup(&qualified)
                    .ok_or_else(|| format!("Unknown native: {}", qualified))?;
                let is_owning = self.rc_enabled && registry.is_owning(native_id);

                let arg_vals: Vec<CValue> = args
                    .iter()
                    .map(|a| self.compile_expr(a))
                    .collect::<Result<Vec<_>, _>>()?;

                let args_addr = self.spill_to_stack(&arg_vals);
                let registry_ptr = self
                    .builder
                    .ins()
                    .iconst(self.ptr_type, registry as *const NativeRegistry as i64);
                let id_val = self.builder.ins().iconst(types::I64, native_id as i64);
                let count_val = self.builder.ins().iconst(types::I64, arg_vals.len() as i64);

                let helper = if is_owning {
                    "jit_call_native_owning"
                } else {
                    "jit_call_native"
                };
                Ok(self.call_helper(helper, &[registry_ptr, id_val, args_addr, count_val]))
            }

            Expr::Concat(parts) => {
                let part_vals = self.compile_exprs_stashed(parts)?;

                let addr = self.spill_to_stack(&part_vals);
                let count = self
                    .builder
                    .ins()
                    .iconst(types::I64, part_vals.len() as i64);
                Ok(self.call_helper("jit_concat", &[addr, count]))
            }

            // -- Phase 3: Constructors --
            Expr::MakeEnum { tag, payload, .. } => {
                if let Some(id) = self.tags.get_id(tag) {
                    // Multi-arg: MakeEnum { payload: MakeTuple([...]) } → flat enum
                    if let Expr::MakeTuple(items, _) = payload.as_ref() {
                        let item_vals = self.compile_exprs_stashed(items)?;
                        let tag_val = self.emit_string_value(tag)?;
                        let id_val = self.builder.ins().iconst(types::I64, id as i64);
                        let addr = self.spill_to_stack(&item_vals);
                        let count = self
                            .builder
                            .ins()
                            .iconst(types::I64, item_vals.len() as i64);
                        return Ok(
                            self.call_helper("jit_make_enum_flat", &[tag_val, id_val, addr, count])
                        );
                    }
                    // Nullary: MakeEnum { payload: Unit } → baked constant
                    if matches!(payload.as_ref(), Expr::Unit) {
                        let tag_str: RcStr = tag.as_str().into();
                        let nv = NValue::enum_val_flat(tag_str, id, vec![]);
                        return Ok(self.emit_heap_nvalue(nv));
                    }
                    // Single-arg: existing path
                    let tag_val = self.emit_string_value(tag)?;
                    let tag_val = self.stash_in_var(tag_val);
                    let payload_val = self.compile_expr(payload)?;
                    let id_val = self.builder.ins().iconst(types::I64, id as i64);
                    Ok(self.call_helper("jit_make_enum_with_id", &[tag_val, id_val, payload_val]))
                } else {
                    let tag_val = self.emit_string_value(tag)?;
                    let tag_val = self.stash_in_var(tag_val);
                    let payload_val = self.compile_expr(payload)?;
                    Ok(self.call_helper("jit_make_enum", &[tag_val, payload_val]))
                }
            }

            Expr::MakeStruct { name, fields, .. } => {
                let name_val = self.emit_string_value(name)?;
                let name_val = self.stash_in_var(name_val);

                let mut pair_vals = Vec::with_capacity(fields.len() * 2);
                for (fname, fexpr) in fields {
                    let key_val = self.emit_string_value(fname)?;
                    let key_val = self.stash_in_var(key_val);
                    let val = self.compile_expr(fexpr)?;
                    pair_vals.push(key_val);
                    pair_vals.push(val);
                }

                let addr = self.spill_to_stack(&pair_vals);
                let count = self.builder.ins().iconst(types::I64, fields.len() as i64);
                Ok(self.call_helper("jit_make_struct", &[name_val, addr, count]))
            }

            Expr::MakeList(items, _) => {
                let item_vals = if items.is_empty() {
                    vec![]
                } else {
                    self.compile_exprs_stashed(items)?
                };

                let addr = self.spill_to_stack(&item_vals);
                let count = self
                    .builder
                    .ins()
                    .iconst(types::I64, item_vals.len() as i64);
                Ok(self.call_helper("jit_make_list", &[addr, count]))
            }

            Expr::MakeRecord(fields, _) => {
                let mut pair_vals = Vec::with_capacity(fields.len() * 2);
                for (fname, fexpr) in fields {
                    let key_val = self.emit_string_value(fname)?;
                    let key_val = self.stash_in_var(key_val);
                    let val = self.compile_expr(fexpr)?;
                    pair_vals.push(key_val);
                    pair_vals.push(val);
                }

                let addr = self.spill_to_stack(&pair_vals);
                let count = self.builder.ins().iconst(types::I64, fields.len() as i64);
                Ok(self.call_helper("jit_make_record", &[addr, count]))
            }

            Expr::MakeTuple(items, _) => {
                let item_vals = if items.is_empty() {
                    vec![]
                } else {
                    self.compile_exprs_stashed(items)?
                };

                let addr = self.spill_to_stack(&item_vals);
                let count = self
                    .builder
                    .ins()
                    .iconst(types::I64, item_vals.len() as i64);
                Ok(self.call_helper("jit_make_tuple", &[addr, count]))
            }

            Expr::MakeRange(start, end) => {
                let start_val = self.compile_expr(start)?;
                let start_val = self.stash_in_var(start_val);
                let end_val = self.compile_expr(end)?;
                Ok(self.call_helper("jit_make_range", &[start_val, end_val]))
            }

            Expr::UpdateRecord {
                base, updates, ty, ..
            } => {
                // Indexed path: if we know the base type, resolve field indices.
                let indices: Option<Vec<u16>> = ty.as_ref().and_then(|t| {
                    updates
                        .iter()
                        .map(|(fname, _)| Self::sorted_field_index(t, fname))
                        .collect()
                });

                let base_val = self.compile_expr(base)?;
                let base_val = self.stash_in_var(base_val);

                if let Some(ref idxs) = indices {
                    let mut update_vals = Vec::with_capacity(updates.len());
                    for (i, (_, fexpr)) in updates.iter().enumerate() {
                        let val = self.compile_expr(fexpr)?;
                        if i + 1 < updates.len() {
                            update_vals.push(self.stash_in_var(val));
                        } else {
                            update_vals.push(val);
                        }
                    }

                    // Fixed-arity fast path used by nbody spread updates.
                    if idxs.len() == 3 {
                        let i0 = self.builder.ins().iconst(types::I64, idxs[0] as i64);
                        let i1 = self.builder.ins().iconst(types::I64, idxs[1] as i64);
                        let i2 = self.builder.ins().iconst(types::I64, idxs[2] as i64);
                        return Ok(self.call_helper(
                            "jit_update_record_indexed3",
                            &[
                                base_val,
                                i0,
                                update_vals[0],
                                i1,
                                update_vals[1],
                                i2,
                                update_vals[2],
                            ],
                        ));
                    }

                    let mut pair_vals = Vec::with_capacity(updates.len() * 2);
                    for (i, val) in update_vals.iter().enumerate() {
                        let idx_val = self.builder.ins().iconst(types::I64, idxs[i] as i64);
                        pair_vals.push(idx_val);
                        pair_vals.push(*val);
                    }
                    let addr = self.spill_to_stack(&pair_vals);
                    let count = self.builder.ins().iconst(types::I64, updates.len() as i64);
                    Ok(self.call_helper("jit_update_record_indexed", &[base_val, addr, count]))
                } else {
                    // Fallback: string-based update
                    let mut pair_vals = Vec::with_capacity(updates.len() * 2);
                    for (fname, fexpr) in updates {
                        let key_val = self.emit_string_value(fname)?;
                        let key_val = self.stash_in_var(key_val);
                        let val = self.compile_expr(fexpr)?;
                        pair_vals.push(key_val);
                        pair_vals.push(val);
                    }
                    let addr = self.spill_to_stack(&pair_vals);
                    let count = self.builder.ins().iconst(types::I64, updates.len() as i64);
                    Ok(self.call_helper("jit_update_record", &[base_val, addr, count]))
                }
            }

            Expr::GetField {
                object,
                field,
                field_idx,
                ..
            } => {
                // SRA: if object is a scalar-replaced record, read field variable directly
                if let Expr::Var(name, _) = object.as_ref()
                    && let Some(field_vars) = self.sra_records.get(name.as_str())
                    && let Some(&var) = field_vars.get(field.as_str())
                {
                    return Ok(self.builder.use_var(var));
                }
                // Indexed fast path: O(1) field access by compile-time index.
                if let Some(idx) = field_idx {
                    let obj_val = self.compile_expr(object)?;
                    let idx_val = self.builder.ins().iconst(types::I64, *idx as i64);
                    let result = self.call_helper("jit_get_field_idx", &[obj_val, idx_val]);
                    if self.rc_enabled {
                        self.emit_decref(obj_val);
                    }
                    return Ok(result);
                }
                // JIT fast path: borrow object variable.
                // (String-based GetField fallback when no field_idx available)
                let obj_val = self.compile_expr(object)?;
                let field_val = self.emit_string_value(field)?;
                let result = self.call_helper("jit_get_field", &[obj_val, field_val]);
                // RC: jit_get_field borrows both args; decref them
                if self.rc_enabled {
                    self.emit_decref(obj_val);
                    self.emit_decref(field_val);
                }
                Ok(result)
            }

            // -- Phase 4: Match --
            Expr::Match { subject, arms, .. } => self.compile_match(subject, arms),

            // -- Phase 5: For, Try --
            Expr::For {
                binding,
                iterable,
                body,
            } => self.compile_for(binding, iterable, body),

            Expr::Try { expr: inner, .. } => self.compile_try(inner),

            Expr::MakeClosure { func_idx, captures } => {
                if captures.is_empty() {
                    // Zero-capture: emit NValue::function directly (no heap allocation)
                    Ok(self.emit_heap_nvalue(NValue::function(*func_idx)))
                } else {
                    // Compile each capture expression
                    let cap_vals = self.compile_exprs_stashed(captures)?;
                    let addr = self.spill_to_stack(&cap_vals);
                    let idx_val = self.builder.ins().iconst(types::I64, *func_idx as i64);
                    let count_val = self.builder.ins().iconst(types::I64, captures.len() as i64);
                    Ok(self.call_helper("jit_make_closure", &[idx_val, addr, count_val]))
                }
            }

            Expr::GetClosureVar(idx) => {
                // __closure is the first parameter (param_vars[0])
                let closure_param = self.builder.use_var(self.param_vars[0]);
                let idx_val = self.builder.ins().iconst(types::I64, *idx as i64);
                Ok(self.call_helper("jit_closure_upvalue", &[closure_param, idx_val]))
            }

            Expr::CallIndirect { callee, args, .. } => {
                let callee_val = self.compile_expr(callee)?;
                let callee_val = self.stash_in_var(callee_val);
                let compiled_args = if args.is_empty() {
                    vec![]
                } else {
                    self.compile_exprs_stashed(args)?
                };
                // RC: compile_call_value handles callee_val cleanup internally:
                // - Closure path: callee passed as param[0], callee function decrefs via pop_rc_scope
                // - Function path: callee NOT passed, decref emitted inside compile_call_value
                let result = self.compile_call_value(callee_val, &compiled_args)?;
                Ok(result)
            }

            Expr::TailCallIndirect { callee, args, .. } => {
                let callee_val = self.compile_expr(callee)?;
                let callee_val = self.stash_in_var(callee_val);
                let compiled_args = if args.is_empty() {
                    vec![]
                } else {
                    self.compile_exprs_stashed(args)?
                };

                let use_tail = self.func_call_conv == CallConv::Tail;

                // Check if closure or plain function
                let is_closure = self.call_helper("jit_is_closure", &[callee_val]);
                let zero = self.builder.ins().iconst(types::I64, 0);
                let cmp = self.builder.ins().icmp(IntCC::NotEqual, is_closure, zero);

                let closure_block = self.builder.create_block();
                let function_block = self.builder.create_block();
                self.builder
                    .ins()
                    .brif(cmp, closure_block, &[], function_block, &[]);

                if use_tail {
                    // Tail CC: use return_call_indirect for proper tail calls
                    // Closure path
                    self.builder.switch_to_block(closure_block);
                    self.builder.seal_block(closure_block);
                    let fn_ptr_c = self.call_helper("jit_closure_fn_ptr", &[callee_val]);
                    let zero_c = self.builder.ins().iconst(types::I64, 0);
                    let ptr_ok_c = self.builder.ins().icmp(IntCC::NotEqual, fn_ptr_c, zero_c);
                    let call_closure_block = self.builder.create_block();
                    let null_closure_block = self.builder.create_block();
                    self.builder.ins().brif(
                        ptr_ok_c,
                        call_closure_block,
                        &[],
                        null_closure_block,
                        &[],
                    );

                    self.builder.switch_to_block(null_closure_block);
                    self.builder.seal_block(null_closure_block);
                    let unit_c = self.builder.ins().iconst(types::I64, NV_UNIT as i64);
                    self.builder.ins().return_(&[unit_c]);

                    self.builder.switch_to_block(call_closure_block);
                    self.builder.seal_block(call_closure_block);
                    let mut cargs = vec![callee_val];
                    cargs.extend(&compiled_args);
                    let closure_sig = self.build_indirect_sig(args.len() + 1);
                    let sig_ref_c = self.builder.import_signature(closure_sig);
                    self.builder
                        .ins()
                        .return_call_indirect(sig_ref_c, fn_ptr_c, &cargs);

                    // Function path
                    self.builder.switch_to_block(function_block);
                    self.builder.seal_block(function_block);
                    let fn_ptr_f = self.call_helper("jit_function_fn_ptr", &[callee_val]);
                    if self.rc_enabled {
                        self.emit_decref(callee_val);
                    }
                    let zero_f = self.builder.ins().iconst(types::I64, 0);
                    let ptr_ok_f = self.builder.ins().icmp(IntCC::NotEqual, fn_ptr_f, zero_f);
                    let call_func_block = self.builder.create_block();
                    let null_func_block = self.builder.create_block();
                    self.builder
                        .ins()
                        .brif(ptr_ok_f, call_func_block, &[], null_func_block, &[]);

                    self.builder.switch_to_block(null_func_block);
                    self.builder.seal_block(null_func_block);
                    let unit_f = self.builder.ins().iconst(types::I64, NV_UNIT as i64);
                    self.builder.ins().return_(&[unit_f]);

                    self.builder.switch_to_block(call_func_block);
                    self.builder.seal_block(call_func_block);
                    let func_sig = self.build_indirect_sig(args.len());
                    let sig_ref_f = self.builder.import_signature(func_sig);
                    self.builder
                        .ins()
                        .return_call_indirect(sig_ref_f, fn_ptr_f, &compiled_args);

                    let dead_block = self.builder.create_block();
                    self.builder.switch_to_block(dead_block);
                    self.builder.seal_block(dead_block);
                    Ok(self.builder.ins().iconst(types::I64, 0))
                } else {
                    // Non-Tail CC (AOT Fast): use call_indirect + return_
                    let merge_block = self.builder.create_block();
                    self.builder.append_block_param(merge_block, types::I64);

                    // Closure path
                    self.builder.switch_to_block(closure_block);
                    self.builder.seal_block(closure_block);
                    let fn_ptr_c = self.call_helper("jit_closure_fn_ptr", &[callee_val]);
                    let zero_c = self.builder.ins().iconst(types::I64, 0);
                    let ptr_ok_c = self.builder.ins().icmp(IntCC::NotEqual, fn_ptr_c, zero_c);
                    let call_closure_block = self.builder.create_block();
                    let null_closure_block = self.builder.create_block();
                    self.builder.ins().brif(
                        ptr_ok_c,
                        call_closure_block,
                        &[],
                        null_closure_block,
                        &[],
                    );

                    self.builder.switch_to_block(null_closure_block);
                    self.builder.seal_block(null_closure_block);
                    let unit_c = self.builder.ins().iconst(types::I64, NV_UNIT as i64);
                    self.builder
                        .ins()
                        .jump(merge_block, &[BlockArg::Value(unit_c)]);

                    self.builder.switch_to_block(call_closure_block);
                    self.builder.seal_block(call_closure_block);
                    let mut cargs = vec![callee_val];
                    cargs.extend(&compiled_args);
                    let closure_sig = self.build_indirect_sig(args.len() + 1);
                    let sig_ref_c = self.builder.import_signature(closure_sig);
                    let inst_c = self
                        .builder
                        .ins()
                        .call_indirect(sig_ref_c, fn_ptr_c, &cargs);
                    let result_c = self.builder.inst_results(inst_c)[0];
                    self.builder
                        .ins()
                        .jump(merge_block, &[BlockArg::Value(result_c)]);

                    // Function path
                    self.builder.switch_to_block(function_block);
                    self.builder.seal_block(function_block);
                    let fn_ptr_f = self.call_helper("jit_function_fn_ptr", &[callee_val]);
                    if self.rc_enabled {
                        self.emit_decref(callee_val);
                    }
                    let zero_f = self.builder.ins().iconst(types::I64, 0);
                    let ptr_ok_f = self.builder.ins().icmp(IntCC::NotEqual, fn_ptr_f, zero_f);
                    let call_func_block = self.builder.create_block();
                    let null_func_block = self.builder.create_block();
                    self.builder
                        .ins()
                        .brif(ptr_ok_f, call_func_block, &[], null_func_block, &[]);

                    self.builder.switch_to_block(null_func_block);
                    self.builder.seal_block(null_func_block);
                    let unit_f = self.builder.ins().iconst(types::I64, NV_UNIT as i64);
                    self.builder
                        .ins()
                        .jump(merge_block, &[BlockArg::Value(unit_f)]);

                    self.builder.switch_to_block(call_func_block);
                    self.builder.seal_block(call_func_block);
                    let func_sig = self.build_indirect_sig(args.len());
                    let sig_ref_f = self.builder.import_signature(func_sig);
                    let inst_f =
                        self.builder
                            .ins()
                            .call_indirect(sig_ref_f, fn_ptr_f, &compiled_args);
                    let result_f = self.builder.inst_results(inst_f)[0];
                    self.builder
                        .ins()
                        .jump(merge_block, &[BlockArg::Value(result_f)]);

                    // Merge
                    self.builder.switch_to_block(merge_block);
                    self.builder.seal_block(merge_block);
                    let result = self.builder.block_params(merge_block)[0];
                    self.builder.ins().return_(&[result]);

                    let dead_block = self.builder.create_block();
                    self.builder.switch_to_block(dead_block);
                    self.builder.seal_block(dead_block);
                    Ok(self.builder.ins().iconst(types::I64, 0))
                }
            }

            Expr::Expect { actual, matcher } => self.compile_expect(actual, matcher),

            // -- Perceus reuse analysis --
            Expr::Drop { name, token, body } => {
                // Load the variable being dropped
                let var_val = if let Some(&var) = self.vars.get(name) {
                    self.builder.use_var(var)
                } else {
                    return Err(format!("Drop: unknown variable '{}'", name));
                };

                // Call jit_drop_reuse to drain inner fields, get reuse token
                let reuse_bits = self.call_helper("jit_drop_reuse", &[var_val]);

                // Null out the consumed variable so function-level pop_rc_scope
                // decrefs UNIT (a no-op) instead of the reused allocation.
                // Same pattern as CoW (jit_enum_field_set) which nulls subj_var.
                // Without this, the return value aliases the consumed param,
                // causing over-decrement when both the function exit and the
                // caller's scope exit decref the same Rc allocation.
                if let Some(&var) = self.vars.get(name) {
                    let unit = self.builder.ins().iconst(types::I64, NV_UNIT as i64);
                    self.builder.def_var(var, unit);
                }

                // Bind the reuse token if requested
                if let Some(tok_name) = token {
                    let tok_var = self.builder.declare_var(types::I64);
                    self.builder.def_var(tok_var, reuse_bits);
                    self.vars.insert(tok_name.clone(), tok_var);
                }

                // Compile the continuation body
                self.compile_expr(body)
            }

            Expr::Reuse { token, alloc } => {
                // Load the reuse token
                let token_val = if let Some(&var) = self.vars.get(token) {
                    self.builder.use_var(var)
                } else {
                    return Err(format!("Reuse: unknown token '{}'", token));
                };

                // Compile the inner allocation with reuse variants
                match alloc.as_ref() {
                    Expr::MakeEnum { tag, payload, .. } => {
                        if let Some(id) = self.tags.get_id(tag) {
                            if let Expr::MakeTuple(items, _) = payload.as_ref() {
                                // Multi-arg flat enum reuse
                                let item_vals = self.compile_exprs_stashed(items)?;
                                let tag_val = self.emit_string_value(tag)?;
                                let id_val = self.builder.ins().iconst(types::I64, id as i64);
                                let addr = self.spill_to_stack(&item_vals);
                                let count = self
                                    .builder
                                    .ins()
                                    .iconst(types::I64, item_vals.len() as i64);
                                Ok(self.call_helper(
                                    "jit_make_enum_flat_reuse",
                                    &[token_val, tag_val, id_val, addr, count],
                                ))
                            } else {
                                // Single-arg enum reuse
                                let tag_val = self.emit_string_value(tag)?;
                                let tag_val = self.stash_in_var(tag_val);
                                let payload_val = self.compile_expr(payload)?;
                                let id_val = self.builder.ins().iconst(types::I64, id as i64);
                                Ok(self.call_helper(
                                    "jit_make_enum_reuse",
                                    &[token_val, tag_val, id_val, payload_val],
                                ))
                            }
                        } else {
                            // No tag_id: fall back to non-reuse path
                            self.compile_expr(alloc)
                        }
                    }
                    Expr::MakeRecord(fields, _) => {
                        let mut pair_vals = Vec::with_capacity(fields.len() * 2);
                        for (fname, fexpr) in fields {
                            let key_val = self.emit_string_value(fname)?;
                            let key_val = self.stash_in_var(key_val);
                            let val = self.compile_expr(fexpr)?;
                            pair_vals.push(key_val);
                            pair_vals.push(val);
                        }
                        let addr = self.spill_to_stack(&pair_vals);
                        let count = self.builder.ins().iconst(types::I64, fields.len() as i64);
                        Ok(self.call_helper("jit_make_record_reuse", &[token_val, addr, count]))
                    }
                    Expr::MakeTuple(items, _) => {
                        let item_vals = if items.is_empty() {
                            vec![]
                        } else {
                            self.compile_exprs_stashed(items)?
                        };
                        let addr = self.spill_to_stack(&item_vals);
                        let count = self
                            .builder
                            .ins()
                            .iconst(types::I64, item_vals.len() as i64);
                        Ok(self.call_helper("jit_make_tuple_reuse", &[token_val, addr, count]))
                    }
                    _ => {
                        // Unsupported alloc type for reuse — compile normally
                        self.compile_expr(alloc)
                    }
                }
            }

            // Typed hole: unreachable at runtime if control flow is correct.
            // Emit a trap so the function body still compiles.
            Expr::Hole => {
                let trap = cranelift_codegen::ir::TrapCode::user(1)
                    .ok_or_else(|| "JIT: invalid trap code".to_string())?;
                self.builder.ins().trap(trap);
                // Unreachable, but Cranelift needs a value for the block.
                let dead_block = self.builder.create_block();
                self.builder.switch_to_block(dead_block);
                self.builder.seal_block(dead_block);
                Ok(self.builder.ins().iconst(types::I64, 0))
            }

            // Remaining unsupported constructs
            _ => Err(format!("Unsupported expression in JIT: {:?}", expr)),
        }
    }

    /// Compile an UpdateRecord expression that consumes the base variable.
    /// Used for self-rebinding patterns like `let x = { ..x, field: val }`.
    /// Reads the base WITHOUT incref so the helper gets refcount==1 and can
    /// mutate in place, then nulls the old variable to prevent double-free.
    fn compile_update_record_consuming(
        &mut self,
        expr: &Expr,
        base_name: &str,
    ) -> Result<CValue, String> {
        let Expr::UpdateRecord {
            updates, ty, ..
        } = expr
        else {
            return self.compile_expr(expr);
        };

        let indices: Option<Vec<u16>> = ty.as_ref().and_then(|t| {
            updates
                .iter()
                .map(|(fname, _)| Self::sorted_field_index(t, fname))
                .collect()
        });

        // Read base WITHOUT incref — we're consuming it
        let base_var = *self.vars.get(base_name).unwrap();
        let base_val = self.builder.use_var(base_var);
        let base_val = self.stash_in_var(base_val);

        // Compile update expressions (may read base.field via GetField — safe,
        // they do their own temporary incref/decref)
        if let Some(ref idxs) = indices {
            let mut update_vals = Vec::with_capacity(updates.len());
            for (i, (_, fexpr)) in updates.iter().enumerate() {
                let val = self.compile_expr(fexpr)?;
                if i + 1 < updates.len() {
                    update_vals.push(self.stash_in_var(val));
                } else {
                    update_vals.push(val);
                }
            }

            // Null out old variable BEFORE calling helper — prevents double-free
            // at scope exit (the old var is tracked in RC scope, decref of NV_UNIT
            // is a no-op).
            let null_val = self
                .builder
                .ins()
                .iconst(types::I64, NV_UNIT as i64);
            self.builder.def_var(base_var, null_val);

            if idxs.len() == 3 {
                let i0 = self.builder.ins().iconst(types::I64, idxs[0] as i64);
                let i1 = self.builder.ins().iconst(types::I64, idxs[1] as i64);
                let i2 = self.builder.ins().iconst(types::I64, idxs[2] as i64);
                return Ok(self.call_helper(
                    "jit_update_record_indexed3",
                    &[
                        base_val,
                        i0,
                        update_vals[0],
                        i1,
                        update_vals[1],
                        i2,
                        update_vals[2],
                    ],
                ));
            }

            let mut pair_vals = Vec::with_capacity(updates.len() * 2);
            for (i, val) in update_vals.iter().enumerate() {
                let idx_val = self.builder.ins().iconst(types::I64, idxs[i] as i64);
                pair_vals.push(idx_val);
                pair_vals.push(*val);
            }
            let addr = self.spill_to_stack(&pair_vals);
            let count = self.builder.ins().iconst(types::I64, updates.len() as i64);
            Ok(self.call_helper(
                "jit_update_record_indexed",
                &[base_val, addr, count],
            ))
        } else {
            // Fallback: string-based. Null first, then call.
            let mut pair_vals = Vec::with_capacity(updates.len() * 2);
            for (fname, fexpr) in updates {
                let key_val = self.emit_string_value(fname)?;
                let key_val = self.stash_in_var(key_val);
                let val = self.compile_expr(fexpr)?;
                pair_vals.push(key_val);
                pair_vals.push(val);
            }
            let null_val = self
                .builder
                .ins()
                .iconst(types::I64, NV_UNIT as i64);
            self.builder.def_var(base_var, null_val);
            let addr = self.spill_to_stack(&pair_vals);
            let count = self.builder.ins().iconst(types::I64, updates.len() as i64);
            Ok(self.call_helper("jit_update_record", &[base_val, addr, count]))
        }
    }

    /// Build a Cranelift signature for indirect calls with `n_params` I64 params.
    fn build_indirect_sig(&self, n_params: usize) -> cranelift_codegen::ir::Signature {
        let mut sig = self.module.make_signature();
        sig.call_conv = self.func_call_conv;
        for _ in 0..n_params {
            sig.params.push(AbiParam::new(types::I64));
        }
        sig.returns.push(AbiParam::new(types::I64));
        sig
    }

    // -- Indirect call dispatch (closure-vs-function) --

    /// Call a value that may be a closure or plain function.
    /// Handles: is_closure check → branch → closure path / function path → merge.
    fn compile_call_value(
        &mut self,
        callee_val: CValue,
        args: &[CValue],
    ) -> Result<CValue, String> {
        let is_closure = self.call_helper("jit_is_closure", &[callee_val]);
        let zero = self.builder.ins().iconst(types::I64, 0);
        let cmp = self.builder.ins().icmp(IntCC::NotEqual, is_closure, zero);

        let closure_block = self.builder.create_block();
        let function_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);
        self.builder
            .ins()
            .brif(cmp, closure_block, &[], function_block, &[]);

        // Closure path: call_indirect(sig_N+1, fn_ptr, [closure, args...])
        self.builder.switch_to_block(closure_block);
        self.builder.seal_block(closure_block);
        let fn_ptr_c = self.call_helper("jit_closure_fn_ptr", &[callee_val]);
        let zero_c = self.builder.ins().iconst(types::I64, 0);
        let ptr_ok_c = self.builder.ins().icmp(IntCC::NotEqual, fn_ptr_c, zero_c);
        let call_closure_block = self.builder.create_block();
        let null_closure_block = self.builder.create_block();
        self.builder
            .ins()
            .brif(ptr_ok_c, call_closure_block, &[], null_closure_block, &[]);

        self.builder.switch_to_block(null_closure_block);
        self.builder.seal_block(null_closure_block);
        // RC: null closure path — callee won't run, so decref the closure here
        if self.rc_enabled {
            self.emit_decref(callee_val);
        }
        let unit_c = self.builder.ins().iconst(types::I64, NV_UNIT as i64);
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(unit_c)]);

        self.builder.switch_to_block(call_closure_block);
        self.builder.seal_block(call_closure_block);
        let mut cargs = vec![callee_val];
        cargs.extend(args);
        let closure_sig = self.build_indirect_sig(args.len() + 1);
        let sig_ref_c = self.builder.import_signature(closure_sig);
        let result_c = self
            .builder
            .ins()
            .call_indirect(sig_ref_c, fn_ptr_c, &cargs);
        let ret_c = self.builder.inst_results(result_c)[0];
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(ret_c)]);

        // Function path: call_indirect(sig_N, fn_ptr, [args...])
        self.builder.switch_to_block(function_block);
        self.builder.seal_block(function_block);
        let fn_ptr_f = self.call_helper("jit_function_fn_ptr", &[callee_val]);
        let zero_f = self.builder.ins().iconst(types::I64, 0);
        let ptr_ok_f = self.builder.ins().icmp(IntCC::NotEqual, fn_ptr_f, zero_f);
        let call_func_block = self.builder.create_block();
        let null_func_block = self.builder.create_block();
        self.builder
            .ins()
            .brif(ptr_ok_f, call_func_block, &[], null_func_block, &[]);

        self.builder.switch_to_block(null_func_block);
        self.builder.seal_block(null_func_block);
        // RC: null function path — decref callee since nobody else will
        if self.rc_enabled {
            self.emit_decref(callee_val);
        }
        let unit_f = self.builder.ins().iconst(types::I64, NV_UNIT as i64);
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(unit_f)]);

        self.builder.switch_to_block(call_func_block);
        self.builder.seal_block(call_func_block);
        let func_sig = self.build_indirect_sig(args.len());
        let sig_ref_f = self.builder.import_signature(func_sig);
        let result_f = self.builder.ins().call_indirect(sig_ref_f, fn_ptr_f, args);
        let ret_f = self.builder.inst_results(result_f)[0];
        // RC: function path — callee_val was NOT passed as a param, so decref here
        if self.rc_enabled {
            self.emit_decref(callee_val);
        }
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(ret_f)]);

        // Merge
        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        Ok(self.builder.block_params(merge_block)[0])
    }

    // -- Expect (inline test matchers) --

    /// Compile `expect actual matcher` → Bool NValue.
    fn compile_expect(&mut self, actual: &Expr, matcher: &Matcher) -> Result<CValue, String> {
        match matcher {
            Matcher::Equal(expected) => {
                let actual_val = self.compile_expr(actual)?;
                let actual_val = self.stash_in_var(actual_val);
                let expected_val = self.compile_expr(expected)?;
                Ok(self.call_helper("jit_values_equal", &[actual_val, expected_val]))
            }
            Matcher::BeOk => {
                let actual_val = self.compile_expr(actual)?;
                let tag_id = self.call_helper("jit_enum_tag_id", &[actual_val]);
                let ok_id = self.tags.get_id("Ok").unwrap_or(2) as i64;
                let expected = self.builder.ins().iconst(types::I64, ok_id);
                self.emit_tag_cmp_to_bool(tag_id, expected)
            }
            Matcher::BeSome => {
                let actual_val = self.compile_expr(actual)?;
                let tag_id = self.call_helper("jit_enum_tag_id", &[actual_val]);
                let some_id = self.tags.get_id("Some").unwrap_or(1) as i64;
                let expected = self.builder.ins().iconst(types::I64, some_id);
                self.emit_tag_cmp_to_bool(tag_id, expected)
            }
            Matcher::BeNone => {
                let actual_val = self.compile_expr(actual)?;
                let tag_id = self.call_helper("jit_enum_tag_id", &[actual_val]);
                let none_id = self.tags.get_id("None").unwrap_or(0) as i64;
                let expected = self.builder.ins().iconst(types::I64, none_id);
                self.emit_tag_cmp_to_bool(tag_id, expected)
            }
            Matcher::BeEmpty => {
                let actual_val = self.compile_expr(actual)?;
                let len_nv = self.call_helper("jit_list_length", &[actual_val]);
                let len_raw = self.untag_int(len_nv);
                let zero = self.builder.ins().iconst(types::I64, 0);
                self.emit_tag_cmp_to_bool(len_raw, zero)
            }
            Matcher::HaveLength(expected) => {
                let actual_val = self.compile_expr(actual)?;
                let actual_val = self.stash_in_var(actual_val);
                let len = self.call_helper("jit_list_length", &[actual_val]);
                let expected_val = self.compile_expr(expected)?;
                Ok(self.call_helper("jit_values_equal", &[len, expected_val]))
            }
            Matcher::Contain(expected) => {
                // Use __test_contains native
                let registry = self.natives.ok_or("No native registry for JIT")?;
                if let Some(native_id) = registry.lookup("__test_contains") {
                    let actual_val = self.compile_expr(actual)?;
                    let actual_val = self.stash_in_var(actual_val);
                    let expected_val = self.compile_expr(expected)?;
                    let args_addr = self.spill_to_stack(&[actual_val, expected_val]);
                    let registry_ptr = self
                        .builder
                        .ins()
                        .iconst(self.ptr_type, registry as *const NativeRegistry as i64);
                    let id_val = self.builder.ins().iconst(types::I64, native_id as i64);
                    let count_val = self.builder.ins().iconst(types::I64, 2);
                    Ok(self.call_helper(
                        "jit_call_native",
                        &[registry_ptr, id_val, args_addr, count_val],
                    ))
                } else {
                    // Fallback: simple equality
                    let actual_val = self.compile_expr(actual)?;
                    let actual_val = self.stash_in_var(actual_val);
                    let expected_val = self.compile_expr(expected)?;
                    Ok(self.call_helper("jit_values_equal", &[actual_val, expected_val]))
                }
            }
            Matcher::StartWith(expected) => {
                // Use String.starts_with native
                let registry = self.natives.ok_or("No native registry for JIT")?;
                if let Some(native_id) = registry.lookup("String.starts_with") {
                    let actual_val = self.compile_expr(actual)?;
                    let actual_val = self.stash_in_var(actual_val);
                    let expected_val = self.compile_expr(expected)?;
                    let args_addr = self.spill_to_stack(&[actual_val, expected_val]);
                    let registry_ptr = self
                        .builder
                        .ins()
                        .iconst(self.ptr_type, registry as *const NativeRegistry as i64);
                    let id_val = self.builder.ins().iconst(types::I64, native_id as i64);
                    let count_val = self.builder.ins().iconst(types::I64, 2);
                    Ok(self.call_helper(
                        "jit_call_native",
                        &[registry_ptr, id_val, args_addr, count_val],
                    ))
                } else {
                    // Fallback: simple equality
                    let actual_val = self.compile_expr(actual)?;
                    let actual_val = self.stash_in_var(actual_val);
                    let expected_val = self.compile_expr(expected)?;
                    Ok(self.call_helper("jit_values_equal", &[actual_val, expected_val]))
                }
            }
            Matcher::Satisfy(pred) => {
                // Apply the predicate to the actual value via indirect call
                let pred_val = self.compile_expr(pred)?;
                let pred_val = self.stash_in_var(pred_val);
                let actual_val = self.compile_expr(actual)?;
                self.compile_call_value(pred_val, &[actual_val])
            }
            Matcher::Be(pattern) => {
                // Compile as a match: pattern → true, _ → false
                let arms = vec![
                    MatchArm {
                        pattern: pattern.clone(),
                        guard: None,
                        body: Expr::Bool(true),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        guard: None,
                        body: Expr::Bool(false),
                    },
                ];
                self.compile_match(actual, &arms)
            }
        }
    }

    /// Compare two i64 values for equality and return NV_TRUE or NV_FALSE.
    fn emit_tag_cmp_to_bool(&mut self, a: CValue, b: CValue) -> Result<CValue, String> {
        let true_val = self.builder.ins().iconst(types::I64, NV_TRUE as i64);
        let false_val = self.builder.ins().iconst(types::I64, NV_FALSE as i64);
        let cmp = self.builder.ins().icmp(IntCC::Equal, a, b);
        Ok(self.builder.ins().select(cmp, true_val, false_val))
    }
}
