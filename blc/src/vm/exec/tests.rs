use super::*;

use crate::vm::chunk::{Chunk, CompileError, Op, Program};
use crate::vm::nvalue::NValue;
use crate::vm::value::Value;

use super::frame::CallFrame;

fn run_chunk(chunk: Chunk) -> Value {
    let mut vm = Vm::new();
    vm.execute(&chunk).expect("VM error")
}

fn run_chunk_err(chunk: Chunk) -> CompileError {
    let mut vm = Vm::new();
    vm.execute(&chunk).expect_err("Expected VM error")
}

// -- Arithmetic --

#[test]
fn add_integers() {
    let mut c = Chunk::new();
    let a = c.add_constant(NValue::int(3));
    let b = c.add_constant(NValue::int(4));
    c.emit(Op::LoadConst(a), 1, 0);
    c.emit(Op::LoadConst(b), 1, 0);
    c.emit(Op::Add, 1, 0);
    c.emit(Op::Return, 1, 0);
    assert_eq!(run_chunk(c), Value::Int(7));
}

#[test]
fn sub_integers() {
    let mut c = Chunk::new();
    let a = c.add_constant(NValue::int(10));
    let b = c.add_constant(NValue::int(3));
    c.emit(Op::LoadConst(a), 1, 0);
    c.emit(Op::LoadConst(b), 1, 0);
    c.emit(Op::Sub, 1, 0);
    c.emit(Op::Return, 1, 0);
    assert_eq!(run_chunk(c), Value::Int(7));
}

#[test]
fn mul_integers() {
    let mut c = Chunk::new();
    let a = c.add_constant(NValue::int(6));
    let b = c.add_constant(NValue::int(7));
    c.emit(Op::LoadConst(a), 1, 0);
    c.emit(Op::LoadConst(b), 1, 0);
    c.emit(Op::Mul, 1, 0);
    c.emit(Op::Return, 1, 0);
    assert_eq!(run_chunk(c), Value::Int(42));
}

#[test]
fn div_integers() {
    let mut c = Chunk::new();
    let a = c.add_constant(NValue::int(15));
    let b = c.add_constant(NValue::int(3));
    c.emit(Op::LoadConst(a), 1, 0);
    c.emit(Op::LoadConst(b), 1, 0);
    c.emit(Op::Div, 1, 0);
    c.emit(Op::Return, 1, 0);
    assert_eq!(run_chunk(c), Value::Int(5));
}

#[test]
fn div_by_zero() {
    let mut c = Chunk::new();
    let a = c.add_constant(NValue::int(1));
    let b = c.add_constant(NValue::int(0));
    c.emit(Op::LoadConst(a), 1, 0);
    c.emit(Op::LoadConst(b), 1, 0);
    c.emit(Op::Div, 1, 0);
    c.emit(Op::Return, 1, 0);
    let err = run_chunk_err(c);
    assert!(err.message.contains("Division by zero"));
}

#[test]
fn modulo() {
    let mut c = Chunk::new();
    let a = c.add_constant(NValue::int(10));
    let b = c.add_constant(NValue::int(3));
    c.emit(Op::LoadConst(a), 1, 0);
    c.emit(Op::LoadConst(b), 1, 0);
    c.emit(Op::Mod, 1, 0);
    c.emit(Op::Return, 1, 0);
    assert_eq!(run_chunk(c), Value::Int(1));
}

#[test]
fn negate_int() {
    let mut c = Chunk::new();
    let a = c.add_constant(NValue::int(5));
    c.emit(Op::LoadConst(a), 1, 0);
    c.emit(Op::Negate, 1, 0);
    c.emit(Op::Return, 1, 0);
    assert_eq!(run_chunk(c), Value::Int(-5));
}

#[test]
fn negate_float() {
    let mut c = Chunk::new();
    let a = c.add_constant(NValue::float(3.125));
    c.emit(Op::LoadConst(a), 1, 0);
    c.emit(Op::Negate, 1, 0);
    c.emit(Op::Return, 1, 0);
    assert_eq!(run_chunk(c), Value::Float(-3.125));
}

// -- Mixed int/float --

#[test]
fn add_int_float() {
    let mut c = Chunk::new();
    let a = c.add_constant(NValue::int(1));
    let b = c.add_constant(NValue::float(2.5));
    c.emit(Op::LoadConst(a), 1, 0);
    c.emit(Op::LoadConst(b), 1, 0);
    c.emit(Op::Add, 1, 0);
    c.emit(Op::Return, 1, 0);
    assert_eq!(run_chunk(c), Value::Float(3.5));
}

// -- Comparisons --

#[test]
fn eq_same() {
    let mut c = Chunk::new();
    let a = c.add_constant(NValue::int(5));
    let b = c.add_constant(NValue::int(5));
    c.emit(Op::LoadConst(a), 1, 0);
    c.emit(Op::LoadConst(b), 1, 0);
    c.emit(Op::Eq, 1, 0);
    c.emit(Op::Return, 1, 0);
    assert_eq!(run_chunk(c), Value::Bool(true));
}

#[test]
fn lt_integers() {
    let mut c = Chunk::new();
    let a = c.add_constant(NValue::int(3));
    let b = c.add_constant(NValue::int(5));
    c.emit(Op::LoadConst(a), 1, 0);
    c.emit(Op::LoadConst(b), 1, 0);
    c.emit(Op::Lt, 1, 0);
    c.emit(Op::Return, 1, 0);
    assert_eq!(run_chunk(c), Value::Bool(true));
}

#[test]
fn not_true() {
    let mut c = Chunk::new();
    let a = c.add_constant(NValue::bool(true));
    c.emit(Op::LoadConst(a), 1, 0);
    c.emit(Op::Not, 1, 0);
    c.emit(Op::Return, 1, 0);
    assert_eq!(run_chunk(c), Value::Bool(false));
}

// -- String concat --

#[test]
fn concat_strings() {
    let mut c = Chunk::new();
    let a = c.add_constant(NValue::string("hello ".into()));
    let b = c.add_constant(NValue::string("world".into()));
    c.emit(Op::LoadConst(a), 1, 0);
    c.emit(Op::LoadConst(b), 1, 0);
    c.emit(Op::Concat, 1, 0);
    c.emit(Op::Return, 1, 0);
    assert_eq!(run_chunk(c), Value::String("hello world".into()));
}

#[test]
fn concat_int_to_string() {
    let mut c = Chunk::new();
    let a = c.add_constant(NValue::string("count: ".into()));
    let b = c.add_constant(NValue::int(42));
    c.emit(Op::LoadConst(a), 1, 0);
    c.emit(Op::LoadConst(b), 1, 0);
    c.emit(Op::Concat, 1, 0);
    c.emit(Op::Return, 1, 0);
    assert_eq!(run_chunk(c), Value::String("count: 42".into()));
}

// -- Jumps --

#[test]
fn jump_unconditional() {
    let mut c = Chunk::new();
    let a = c.add_constant(NValue::int(1));
    let b = c.add_constant(NValue::int(99));
    c.emit(Op::Jump(1), 1, 0);
    c.emit(Op::LoadConst(a), 1, 0);
    c.emit(Op::LoadConst(b), 1, 0);
    c.emit(Op::Return, 1, 0);
    assert_eq!(run_chunk(c), Value::Int(99));
}

#[test]
fn jump_if_false_taken() {
    let mut c = Chunk::new();
    let f = c.add_constant(NValue::bool(false));
    let a = c.add_constant(NValue::int(1));
    let b = c.add_constant(NValue::int(2));
    c.emit(Op::LoadConst(f), 1, 0);
    c.emit(Op::JumpIfFalse(1), 1, 0);
    c.emit(Op::LoadConst(a), 1, 0);
    c.emit(Op::LoadConst(b), 1, 0);
    c.emit(Op::Return, 1, 0);
    assert_eq!(run_chunk(c), Value::Int(2));
}

#[test]
fn jump_if_false_not_taken() {
    let mut c = Chunk::new();
    let t = c.add_constant(NValue::bool(true));
    let a = c.add_constant(NValue::int(1));
    c.emit(Op::LoadConst(t), 1, 0);
    c.emit(Op::JumpIfFalse(1), 1, 0);
    c.emit(Op::LoadConst(a), 1, 0);
    c.emit(Op::Return, 1, 0);
    assert_eq!(run_chunk(c), Value::Int(1));
}

// -- Compound expression: 1 + 2 * 3 = 7 --

#[test]
fn order_of_operations() {
    let mut c = Chunk::new();
    let one = c.add_constant(NValue::int(1));
    let two = c.add_constant(NValue::int(2));
    let three = c.add_constant(NValue::int(3));
    c.emit(Op::LoadConst(one), 1, 0);
    c.emit(Op::LoadConst(two), 1, 0);
    c.emit(Op::LoadConst(three), 1, 0);
    c.emit(Op::Mul, 1, 0);
    c.emit(Op::Add, 1, 0);
    c.emit(Op::Return, 1, 0);
    assert_eq!(run_chunk(c), Value::Int(7));
}

// -- Function calls --

#[test]
fn call_simple_function() {
    let mut caller = Chunk::new();
    let func = caller.add_constant(NValue::function(1));
    let arg = caller.add_constant(NValue::int(5));
    caller.emit(Op::LoadConst(func), 1, 0);
    caller.emit(Op::LoadConst(arg), 1, 0);
    caller.emit(Op::Call(1), 1, 0);
    caller.emit(Op::Return, 1, 0);

    let mut callee = Chunk::new();
    let two = callee.add_constant(NValue::int(2));
    callee.emit(Op::GetLocal(0), 1, 0);
    callee.emit(Op::LoadConst(two), 1, 0);
    callee.emit(Op::Mul, 1, 0);
    callee.emit(Op::Return, 1, 0);

    let program = Program {
        chunks: vec![caller, callee],
        entry: 0,
    };
    let mut vm = Vm::new();
    assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(10));
}

#[test]
fn call_two_arg_function() {
    let mut caller = Chunk::new();
    let func = caller.add_constant(NValue::function(1));
    let a = caller.add_constant(NValue::int(3));
    let b = caller.add_constant(NValue::int(4));
    caller.emit(Op::LoadConst(func), 1, 0);
    caller.emit(Op::LoadConst(a), 1, 0);
    caller.emit(Op::LoadConst(b), 1, 0);
    caller.emit(Op::Call(2), 1, 0);
    caller.emit(Op::Return, 1, 0);

    let mut callee = Chunk::new();
    callee.emit(Op::GetLocal(0), 1, 0);
    callee.emit(Op::GetLocal(1), 1, 0);
    callee.emit(Op::Add, 1, 0);
    callee.emit(Op::Return, 1, 0);

    let program = Program {
        chunks: vec![caller, callee],
        entry: 0,
    };
    let mut vm = Vm::new();
    assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(7));
}

#[test]
fn nested_calls() {
    let mut entry = Chunk::new();
    let add_fn = entry.add_constant(NValue::function(1));
    let dbl_fn = entry.add_constant(NValue::function(2));
    let three = entry.add_constant(NValue::int(3));
    let four = entry.add_constant(NValue::int(4));
    entry.emit(Op::LoadConst(add_fn), 1, 0);
    entry.emit(Op::LoadConst(dbl_fn), 1, 0);
    entry.emit(Op::LoadConst(three), 1, 0);
    entry.emit(Op::Call(1), 1, 0);
    entry.emit(Op::LoadConst(four), 1, 0);
    entry.emit(Op::Call(2), 1, 0);
    entry.emit(Op::Return, 1, 0);

    let mut add = Chunk::new();
    add.emit(Op::GetLocal(0), 1, 0);
    add.emit(Op::GetLocal(1), 1, 0);
    add.emit(Op::Add, 1, 0);
    add.emit(Op::Return, 1, 0);

    let mut dbl = Chunk::new();
    let two = dbl.add_constant(NValue::int(2));
    dbl.emit(Op::GetLocal(0), 1, 0);
    dbl.emit(Op::LoadConst(two), 1, 0);
    dbl.emit(Op::Mul, 1, 0);
    dbl.emit(Op::Return, 1, 0);

    let program = Program {
        chunks: vec![entry, add, dbl],
        entry: 0,
    };
    let mut vm = Vm::new();
    assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(10));
}

#[test]
fn get_field_missing_returns_error() {
    let mut c = Chunk::new();
    let key = c.add_constant(NValue::string("x".into()));
    let val = c.add_constant(NValue::int(1));
    let bad_field = c.add_constant(NValue::string("y".into()));
    c.emit(Op::LoadConst(key), 1, 0);
    c.emit(Op::LoadConst(val), 1, 0);
    c.emit(Op::MakeRecord(1), 1, 0);
    c.emit(Op::GetField(bad_field), 1, 0);
    c.emit(Op::Return, 1, 0);
    let err = run_chunk_err(c);
    assert!(err.message.contains("no field 'y'"), "got: {}", err.message);
}

#[test]
fn stack_overflow_detected() {
    let mut entry = Chunk::new();
    let func = entry.add_constant(NValue::function(1));
    entry.emit(Op::LoadConst(func), 1, 0);
    entry.emit(Op::Call(0), 1, 0);
    entry.emit(Op::Return, 1, 0);

    let mut recurse = Chunk::new();
    let self_func = recurse.add_constant(NValue::function(1));
    recurse.emit(Op::LoadConst(self_func), 1, 0);
    recurse.emit(Op::Call(0), 1, 0);
    recurse.emit(Op::Return, 1, 0);

    let program = Program {
        chunks: vec![entry, recurse],
        entry: 0,
    };
    let mut vm = Vm::new();
    let err = vm
        .execute_program(&program)
        .expect_err("Expected stack overflow");
    assert!(
        err.message.contains("Stack overflow"),
        "got: {}",
        err.message
    );
}

// -- Superinstruction tests --

#[test]
fn superinstruction_get_local_sub_int() {
    // Test that GetLocalSubInt produces the same result as the unfused sequence:
    // GetLocal(0) + LoadSmallInt(1) + SubInt
    let mut c_unfused = Chunk::new();
    let arg = c_unfused.add_constant(NValue::int(10));
    c_unfused.emit(Op::LoadConst(arg), 1, 0); // push 10 as local slot 0
    c_unfused.emit(Op::GetLocal(0), 1, 0);
    c_unfused.emit(Op::LoadSmallInt(1), 1, 0);
    c_unfused.emit(Op::SubInt, 1, 0);
    c_unfused.emit(Op::Return, 1, 0);
    let unfused_result = run_chunk(c_unfused);

    let mut c_fused = Chunk::new();
    let arg = c_fused.add_constant(NValue::int(10));
    c_fused.emit(Op::LoadConst(arg), 1, 0); // push 10 as local slot 0
    c_fused.emit(Op::GetLocalSubInt(0, 1), 1, 0);
    c_fused.emit(Op::Return, 1, 0);
    let fused_result = run_chunk(c_fused);

    assert_eq!(unfused_result, Value::Int(9));
    assert_eq!(fused_result, Value::Int(9));
    assert_eq!(unfused_result, fused_result);
}

#[test]
fn superinstruction_get_local_sub_int_negative() {
    // Test subtraction with a negative small int (e.g., n - (-3) = n + 3)
    let mut c = Chunk::new();
    let arg = c.add_constant(NValue::int(5));
    c.emit(Op::LoadConst(arg), 1, 0);
    c.emit(Op::GetLocalSubInt(0, -3), 1, 0);
    c.emit(Op::Return, 1, 0);
    assert_eq!(run_chunk(c), Value::Int(8));
}

#[test]
fn superinstruction_get_local_le_int() {
    // Test that GetLocalLeInt produces the same result as the unfused sequence:
    // GetLocal(0) + LoadSmallInt(1) + LeInt
    let mut c_unfused = Chunk::new();
    let arg = c_unfused.add_constant(NValue::int(0));
    c_unfused.emit(Op::LoadConst(arg), 1, 0);
    c_unfused.emit(Op::GetLocal(0), 1, 0);
    c_unfused.emit(Op::LoadSmallInt(1), 1, 0);
    c_unfused.emit(Op::LeInt, 1, 0);
    c_unfused.emit(Op::Return, 1, 0);
    let unfused_result = run_chunk(c_unfused);

    let mut c_fused = Chunk::new();
    let arg = c_fused.add_constant(NValue::int(0));
    c_fused.emit(Op::LoadConst(arg), 1, 0);
    c_fused.emit(Op::GetLocalLeInt(0, 1), 1, 0);
    c_fused.emit(Op::Return, 1, 0);
    let fused_result = run_chunk(c_fused);

    assert_eq!(unfused_result, Value::Bool(true));
    assert_eq!(fused_result, Value::Bool(true));
    assert_eq!(unfused_result, fused_result);
}

#[test]
fn superinstruction_le_int_false_case() {
    // n = 5, check 5 <= 1 = false
    let mut c = Chunk::new();
    let arg = c.add_constant(NValue::int(5));
    c.emit(Op::LoadConst(arg), 1, 0);
    c.emit(Op::GetLocalLeInt(0, 1), 1, 0);
    c.emit(Op::Return, 1, 0);
    assert_eq!(run_chunk(c), Value::Bool(false));
}

#[test]
fn superinstruction_le_int_equal_case() {
    // n = 1, check 1 <= 1 = true
    let mut c = Chunk::new();
    let arg = c.add_constant(NValue::int(1));
    c.emit(Op::LoadConst(arg), 1, 0);
    c.emit(Op::GetLocalLeInt(0, 1), 1, 0);
    c.emit(Op::Return, 1, 0);
    assert_eq!(run_chunk(c), Value::Bool(true));
}

#[test]
fn superinstruction_in_function_call() {
    // Simulate fibonacci-like pattern: function with arg n,
    // compute n <= 1 and n - 1 using superinstructions
    let mut caller = Chunk::new();
    let func = caller.add_constant(NValue::function(1));
    let arg = caller.add_constant(NValue::int(5));
    caller.emit(Op::LoadConst(func), 1, 0);
    caller.emit(Op::LoadConst(arg), 1, 0);
    caller.emit(Op::Call(1), 1, 0);
    caller.emit(Op::Return, 1, 0);

    // Callee: takes n (slot 0), returns n - 2
    let mut callee = Chunk::new();
    callee.emit(Op::GetLocalSubInt(0, 2), 1, 0);
    callee.emit(Op::Return, 1, 0);

    let program = Program {
        chunks: vec![caller, callee],
        entry: 0,
    };
    let mut vm = Vm::new();
    assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(3));
}

#[test]
fn superinstruction_fusion_produces_correct_result() {
    // Build a chunk with the unfused pattern, run fusion, verify same result
    let mut caller = Chunk::new();
    let func = caller.add_constant(NValue::function(1));
    let arg = caller.add_constant(NValue::int(7));
    caller.emit(Op::LoadConst(func), 1, 0);
    caller.emit(Op::LoadConst(arg), 1, 0);
    caller.emit(Op::Call(1), 1, 0);
    caller.emit(Op::Return, 1, 0);

    // Callee: GetLocal(0) + LoadSmallInt(3) + SubInt => should fuse
    let mut callee = Chunk::new();
    callee.emit(Op::GetLocal(0), 1, 0);
    callee.emit(Op::LoadSmallInt(3), 1, 0);
    callee.emit(Op::SubInt, 1, 0);
    callee.emit(Op::Return, 1, 0);

    let mut program = Program {
        chunks: vec![caller, callee],
        entry: 0,
    };
    program.optimize();

    // After optimization + compaction, callee chunk is [GetLocalSubInt, Return]
    assert_eq!(program.chunks[1].code[0], Op::GetLocalSubInt(0, 3));

    let mut vm = Vm::new();
    assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(4));
}

// -- Tail call optimization tests --

#[test]
fn tail_call_factorial() {
    // fact(n, acc) = if n <= 1 then acc else fact(n-1, n*acc)
    // Entry: call fact(10, 1)
    let mut entry = Chunk::new();
    let func = entry.add_constant(NValue::function(1));
    let n = entry.add_constant(NValue::int(10));
    let acc = entry.add_constant(NValue::int(1));
    entry.emit(Op::LoadConst(func), 1, 0); // push function
    entry.emit(Op::LoadConst(n), 1, 0); // push 10
    entry.emit(Op::LoadConst(acc), 1, 0); // push 1
    entry.emit(Op::Call(2), 1, 0); // call fact(10, 1)
    entry.emit(Op::Return, 1, 0);

    // fact: slot 0 = n, slot 1 = acc
    // if n <= 1 then acc else TailCall(n-1, n*acc)
    let mut fact = Chunk::new();
    let one = fact.add_constant(NValue::int(1));
    // Check n <= 1
    fact.emit(Op::GetLocal(0), 2, 0); // push n
    fact.emit(Op::LoadConst(one), 2, 0); // push 1
    fact.emit(Op::Le, 2, 0); // n <= 1
    let then_jump = fact.emit(Op::JumpIfFalse(0), 2, 0);
    // Then: return acc
    fact.emit(Op::GetLocal(1), 3, 0); // push acc
    let else_jump = fact.emit(Op::Jump(0), 3, 0);
    // Else: TailCall(n-1, n*acc)
    fact.patch_jump(then_jump);
    fact.emit(Op::GetLocal(0), 4, 0); // push n
    fact.emit(Op::LoadConst(one), 4, 0); // push 1
    fact.emit(Op::Sub, 4, 0); // n - 1  (first arg)
    fact.emit(Op::GetLocal(0), 4, 0); // push n
    fact.emit(Op::GetLocal(1), 4, 0); // push acc
    fact.emit(Op::Mul, 4, 0); // n * acc  (second arg)
    fact.emit(Op::TailCall(2), 4, 0); // tail call with 2 args
    fact.patch_jump(else_jump);
    fact.emit(Op::Return, 5, 0);

    let program = Program {
        chunks: vec![entry, fact],
        entry: 0,
    };
    let mut vm = Vm::new();
    assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(3628800));
}

#[test]
fn tail_call_deep_recursion() {
    // countdown(n) = if n <= 0 then 0 else countdown(n-1)
    // Tests that 100,000 iterations don't overflow the call stack
    let mut entry = Chunk::new();
    let func = entry.add_constant(NValue::function(1));
    let n = entry.add_constant(NValue::int(100_000));
    entry.emit(Op::LoadConst(func), 1, 0);
    entry.emit(Op::LoadConst(n), 1, 0);
    entry.emit(Op::Call(1), 1, 0);
    entry.emit(Op::Return, 1, 0);

    // countdown: slot 0 = n
    // if n <= 0 then 0 else TailCall(n-1)
    let mut countdown = Chunk::new();
    let zero = countdown.add_constant(NValue::int(0));
    let one_const = countdown.add_constant(NValue::int(1));
    // Check n <= 0
    countdown.emit(Op::GetLocal(0), 2, 0);
    countdown.emit(Op::LoadConst(zero), 2, 0);
    countdown.emit(Op::Le, 2, 0);
    let then_jump = countdown.emit(Op::JumpIfFalse(0), 2, 0);
    // Then: return 0
    countdown.emit(Op::LoadConst(zero), 3, 0);
    let else_jump = countdown.emit(Op::Jump(0), 3, 0);
    // Else: TailCall(n-1)
    countdown.patch_jump(then_jump);
    countdown.emit(Op::GetLocal(0), 4, 0);
    countdown.emit(Op::LoadConst(one_const), 4, 0);
    countdown.emit(Op::Sub, 4, 0);
    countdown.emit(Op::TailCall(1), 4, 0);
    countdown.patch_jump(else_jump);
    countdown.emit(Op::Return, 5, 0);

    let program = Program {
        chunks: vec![entry, countdown],
        entry: 0,
    };
    let mut vm = Vm::new();
    // Without TCO, 100,000 recursive calls would hit MAX_CALL_DEPTH (1024)
    assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(0));
}

#[test]
fn superinstruction_get_local_add_int() {
    // Test that GetLocalAddInt produces same result as unfused sequence
    let mut c_unfused = Chunk::new();
    let arg = c_unfused.add_constant(NValue::int(10));
    c_unfused.emit(Op::LoadConst(arg), 1, 0);
    c_unfused.emit(Op::GetLocal(0), 1, 0);
    c_unfused.emit(Op::LoadSmallInt(3), 1, 0);
    c_unfused.emit(Op::AddInt, 1, 0);
    c_unfused.emit(Op::Return, 1, 0);
    let unfused = run_chunk(c_unfused);

    let mut c_fused = Chunk::new();
    let arg = c_fused.add_constant(NValue::int(10));
    c_fused.emit(Op::LoadConst(arg), 1, 0);
    c_fused.emit(Op::GetLocalAddInt(0, 3), 1, 0);
    c_fused.emit(Op::Return, 1, 0);
    let fused = run_chunk(c_fused);

    assert_eq!(unfused, Value::Int(13));
    assert_eq!(fused, Value::Int(13));
}

#[test]
fn superinstruction_get_local_lt_int() {
    // Test that GetLocalLtInt produces same result as unfused sequence
    let mut c_unfused = Chunk::new();
    let arg = c_unfused.add_constant(NValue::int(3));
    c_unfused.emit(Op::LoadConst(arg), 1, 0);
    c_unfused.emit(Op::GetLocal(0), 1, 0);
    c_unfused.emit(Op::LoadSmallInt(5), 1, 0);
    c_unfused.emit(Op::LtInt, 1, 0);
    c_unfused.emit(Op::Return, 1, 0);
    let unfused = run_chunk(c_unfused);

    let mut c_fused = Chunk::new();
    let arg = c_fused.add_constant(NValue::int(3));
    c_fused.emit(Op::LoadConst(arg), 1, 0);
    c_fused.emit(Op::GetLocalLtInt(0, 5), 1, 0);
    c_fused.emit(Op::Return, 1, 0);
    let fused = run_chunk(c_fused);

    assert_eq!(unfused, Value::Bool(true));
    assert_eq!(fused, Value::Bool(true));
}

#[test]
fn superinstruction_lt_int_false() {
    // 7 < 5 → false
    let mut c = Chunk::new();
    let arg = c.add_constant(NValue::int(7));
    c.emit(Op::LoadConst(arg), 1, 0);
    c.emit(Op::GetLocalLtInt(0, 5), 1, 0);
    c.emit(Op::Return, 1, 0);
    assert_eq!(run_chunk(c), Value::Bool(false));
}

#[test]
fn superinstruction_lt_int_equal() {
    // 5 < 5 → false (strictly less than)
    let mut c = Chunk::new();
    let arg = c.add_constant(NValue::int(5));
    c.emit(Op::LoadConst(arg), 1, 0);
    c.emit(Op::GetLocalLtInt(0, 5), 1, 0);
    c.emit(Op::Return, 1, 0);
    assert_eq!(run_chunk(c), Value::Bool(false));
}

#[test]
fn tail_call_sum_accumulator() {
    // sum(n, acc) = if n <= 0 then acc else sum(n-1, acc+n)
    // sum(100, 0) = 5050
    let mut entry = Chunk::new();
    let func = entry.add_constant(NValue::function(1));
    let n = entry.add_constant(NValue::int(100));
    let zero = entry.add_constant(NValue::int(0));
    entry.emit(Op::LoadConst(func), 1, 0);
    entry.emit(Op::LoadConst(n), 1, 0);
    entry.emit(Op::LoadConst(zero), 1, 0);
    entry.emit(Op::Call(2), 1, 0);
    entry.emit(Op::Return, 1, 0);

    // sum: slot 0 = n, slot 1 = acc
    let mut sum = Chunk::new();
    let zero_c = sum.add_constant(NValue::int(0));
    let one_c = sum.add_constant(NValue::int(1));
    // if n <= 0
    sum.emit(Op::GetLocal(0), 2, 0);
    sum.emit(Op::LoadConst(zero_c), 2, 0);
    sum.emit(Op::Le, 2, 0);
    let then_jump = sum.emit(Op::JumpIfFalse(0), 2, 0);
    // then acc
    sum.emit(Op::GetLocal(1), 3, 0);
    let else_jump = sum.emit(Op::Jump(0), 3, 0);
    // else TailCall(n-1, acc+n)
    sum.patch_jump(then_jump);
    sum.emit(Op::GetLocal(0), 4, 0);
    sum.emit(Op::LoadConst(one_c), 4, 0);
    sum.emit(Op::Sub, 4, 0); // n - 1
    sum.emit(Op::GetLocal(1), 4, 0);
    sum.emit(Op::GetLocal(0), 4, 0);
    sum.emit(Op::Add, 4, 0); // acc + n
    sum.emit(Op::TailCall(2), 4, 0);
    sum.patch_jump(else_jump);
    sum.emit(Op::Return, 5, 0);

    let program = Program {
        chunks: vec![entry, sum],
        entry: 0,
    };
    let mut vm = Vm::new();
    assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(5050));
}

// -- CallDirect tests --

#[test]
fn call_direct_simple() {
    // Entry calls callee via CallDirect (no function value on stack)
    let mut caller = Chunk::new();
    let arg = caller.add_constant(NValue::int(5));
    caller.emit(Op::LoadConst(arg), 1, 0);
    caller.emit(Op::CallDirect(1, 1), 1, 0);
    caller.emit(Op::Return, 1, 0);

    let mut callee = Chunk::new();
    let two = callee.add_constant(NValue::int(2));
    callee.emit(Op::GetLocal(0), 1, 0);
    callee.emit(Op::LoadConst(two), 1, 0);
    callee.emit(Op::Mul, 1, 0);
    callee.emit(Op::Return, 1, 0);

    let program = Program {
        chunks: vec![caller, callee],
        entry: 0,
    };
    let mut vm = Vm::new();
    assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(10));
}

#[test]
fn call_direct_two_args() {
    let mut caller = Chunk::new();
    let a = caller.add_constant(NValue::int(3));
    let b = caller.add_constant(NValue::int(4));
    caller.emit(Op::LoadConst(a), 1, 0);
    caller.emit(Op::LoadConst(b), 1, 0);
    caller.emit(Op::CallDirect(1, 2), 1, 0);
    caller.emit(Op::Return, 1, 0);

    let mut callee = Chunk::new();
    callee.emit(Op::GetLocal(0), 1, 0);
    callee.emit(Op::GetLocal(1), 1, 0);
    callee.emit(Op::Add, 1, 0);
    callee.emit(Op::Return, 1, 0);

    let program = Program {
        chunks: vec![caller, callee],
        entry: 0,
    };
    let mut vm = Vm::new();
    assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(7));
}

#[test]
fn call_direct_recursive_fib() {
    // fib(n) = if n <= 1 then n else fib(n-1) + fib(n-2)
    // Entry: CallDirect fib(10)
    let mut entry = Chunk::new();
    let arg = entry.add_constant(NValue::int(10));
    entry.emit(Op::LoadConst(arg), 1, 0);
    entry.emit(Op::CallDirect(1, 1), 1, 0);
    entry.emit(Op::Return, 1, 0);

    // fib: slot 0 = n
    let mut fib = Chunk::new();
    let one = fib.add_constant(NValue::int(1));
    let two = fib.add_constant(NValue::int(2));
    // n <= 1
    fib.emit(Op::GetLocal(0), 1, 0);
    fib.emit(Op::LoadConst(one), 1, 0);
    fib.emit(Op::Le, 1, 0);
    let then_jump = fib.emit(Op::JumpIfFalse(0), 1, 0);
    // then: return n
    fib.emit(Op::GetLocal(0), 1, 0);
    let else_jump = fib.emit(Op::Jump(0), 1, 0);
    // else: fib(n-1) + fib(n-2)
    fib.patch_jump(then_jump);
    fib.emit(Op::GetLocal(0), 1, 0);
    fib.emit(Op::LoadConst(one), 1, 0);
    fib.emit(Op::Sub, 1, 0); // n - 1
    fib.emit(Op::CallDirect(1, 1), 1, 0); // fib(n-1)
    fib.emit(Op::GetLocal(0), 1, 0);
    fib.emit(Op::LoadConst(two), 1, 0);
    fib.emit(Op::Sub, 1, 0); // n - 2
    fib.emit(Op::CallDirect(1, 1), 1, 0); // fib(n-2)
    fib.emit(Op::Add, 1, 0);
    fib.patch_jump(else_jump);
    fib.emit(Op::Return, 1, 0);

    let program = Program {
        chunks: vec![entry, fib],
        entry: 0,
    };
    let mut vm = Vm::new();
    assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(55));
}

#[test]
fn call_direct_mixed_with_call() {
    // Test mixing CallDirect (known function) and Call (closure/indirect)
    let mut entry = Chunk::new();
    let func = entry.add_constant(NValue::function(2)); // indirect reference
    let arg = entry.add_constant(NValue::int(5));
    // Direct call to chunk 1
    entry.emit(Op::LoadConst(arg), 1, 0);
    entry.emit(Op::CallDirect(1, 1), 1, 0);
    // Indirect call to chunk 2 via Call
    entry.emit(Op::LoadConst(func), 1, 0);
    entry.emit(Op::Call(0), 1, 0); // Call with result from CallDirect as arg? No, Call(0) pops func, pushes result
    // Add both results  -- actually let's push result of direct call, then indirect call, then add
    entry.emit(Op::Add, 1, 0);
    entry.emit(Op::Return, 1, 0);

    // Chunk 1: double(n) = n * 2
    let mut double = Chunk::new();
    let two = double.add_constant(NValue::int(2));
    double.emit(Op::GetLocal(0), 1, 0);
    double.emit(Op::LoadConst(two), 1, 0);
    double.emit(Op::Mul, 1, 0);
    double.emit(Op::Return, 1, 0);

    // Chunk 2: returns 100
    let mut hundred = Chunk::new();
    let val = hundred.add_constant(NValue::int(100));
    hundred.emit(Op::LoadConst(val), 1, 0);
    hundred.emit(Op::Return, 1, 0);

    let program = Program {
        chunks: vec![entry, double, hundred],
        entry: 0,
    };
    let mut vm = Vm::new();
    // double(5) = 10, hundred() = 100, 10 + 100 = 110
    assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(110));
}

#[test]
fn call_direct_stack_overflow() {
    // Ensure CallDirect still detects stack overflow
    let mut entry = Chunk::new();
    entry.emit(Op::CallDirect(1, 0), 1, 0);
    entry.emit(Op::Return, 1, 0);

    let mut recurse = Chunk::new();
    recurse.emit(Op::CallDirect(1, 0), 1, 0);
    recurse.emit(Op::Return, 1, 0);

    let program = Program {
        chunks: vec![entry, recurse],
        entry: 0,
    };
    let mut vm = Vm::new();
    let err = vm
        .execute_program(&program)
        .expect_err("Expected stack overflow");
    assert!(
        err.message.contains("Stack overflow"),
        "got: {}",
        err.message
    );
}

#[test]
fn call_frame_size() {
    // AC-4.8: CallFrame must remain 16 bytes or smaller
    assert!(
        std::mem::size_of::<CallFrame>() <= 16,
        "CallFrame is {} bytes, must be <= 16",
        std::mem::size_of::<CallFrame>()
    );
}
