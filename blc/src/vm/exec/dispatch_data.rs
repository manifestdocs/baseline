use crate::vm::chunk::{Chunk, CompileError, Op};
use crate::vm::nvalue::{HeapObject, NValue};

use super::{DispatchResult, MAX_RANGE_SIZE};

/// Data structure opcodes: MakeRange, ListGet, ListLen, MakeList, ListConcat,
/// MakeRecord, GetField, MakeTuple, TupleGet, MakeEnum, MakeStruct,
/// EnumTag, EnumPayload, UpdateRecord.
impl super::Vm {
    #[inline(always)]
    pub(crate) fn dispatch_data_structures(
        &mut self,
        op: &Op,
        chunk: &Chunk,
        ip: usize,
    ) -> Result<DispatchResult, CompileError> {
        match op {
            Op::MakeRange => {
                let (end_val, start_val) = self.pop2_fast();
                if start_val.is_any_int() && end_val.is_any_int() {
                    let start = start_val.as_any_int();
                    let end = end_val.as_any_int();
                    let size = end - start;
                    if size > MAX_RANGE_SIZE {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!(
                                "Range too large ({} elements, max {})",
                                size, MAX_RANGE_SIZE
                            ),
                            line,
                            col,
                        ));
                    }
                    let list: Vec<NValue> = (start..end).map(NValue::int).collect();
                    self.stack.push(NValue::list(list));
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("Range requires integers, got {} and {}", start_val, end_val),
                        line,
                        col,
                    ));
                }
            }
            Op::ListGet => {
                let (idx_val, list_val) = self.pop2_fast();
                if list_val.is_heap() && idx_val.is_any_int() {
                    if let HeapObject::List(items) = list_val.as_heap_ref() {
                        let idx_i64 = idx_val.as_any_int();
                        if idx_i64 < 0 {
                            let (line, col) = chunk.source_map[ip - 1];
                            return Err(self.error(
                                format!("Negative index {} (len {})", idx_i64, items.len()),
                                line,
                                col,
                            ));
                        }
                        let idx = idx_i64 as usize;
                        if idx >= items.len() {
                            let (line, col) = chunk.source_map[ip - 1];
                            return Err(self.error(
                                format!("Index {} out of bounds (len {})", idx, items.len()),
                                line,
                                col,
                            ));
                        }
                        self.stack.push(items[idx].clone());
                    } else {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!(
                                "ListGet requires List and Int, got {} and {}",
                                list_val, idx_val
                            ),
                            line,
                            col,
                        ));
                    }
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!(
                            "ListGet requires List and Int, got {} and {}",
                            list_val, idx_val
                        ),
                        line,
                        col,
                    ));
                }
            }
            Op::ListLen => {
                let list_val = self.pop_fast();
                if list_val.is_heap() {
                    if let HeapObject::List(items) = list_val.as_heap_ref() {
                        self.stack.push(NValue::int(items.len() as i64));
                    } else {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("ListLen requires List, got {}", list_val),
                            line,
                            col,
                        ));
                    }
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("ListLen requires List, got {}", list_val),
                        line,
                        col,
                    ));
                }
            }
            Op::MakeList(n) => {
                let count = *n as usize;
                let start = self.stack.len() - count;
                let items: Vec<NValue> = self.stack.drain(start..).collect();
                self.stack.push(NValue::list(items));
            }
            Op::ListConcat => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();

                // Read lengths/items before attempting CoW (borrows are needed first)
                let l_len = left.as_list().unwrap().len();
                let r_len = right.as_list().unwrap().len();

                // Fast path: [] ++ list or list ++ []
                if l_len == 0 {
                    self.stack.push(right);
                } else if r_len == 0 {
                    self.stack.push(left);
                // CoW fast path: [x] ++ list — if right is uniquely owned, insert(0, x) in-place
                } else if l_len == 1 {
                    let x = left.as_list().unwrap()[0].clone();
                    match right.try_unwrap_heap() {
                        Ok(HeapObject::List(mut vec)) => {
                            vec.insert(0, x);
                            self.stack.push(NValue::list(vec));
                        }
                        Ok(_) => unreachable!(),
                        Err(right) => {
                            let r_items = right.as_list().unwrap();
                            let mut result = Vec::with_capacity(1 + r_items.len());
                            result.push(x);
                            result.extend_from_slice(r_items);
                            self.stack.push(NValue::list(result));
                        }
                    }
                // CoW fast path: list ++ [x] — if left is uniquely owned, push(x) in-place
                } else if r_len == 1 {
                    let x = right.as_list().unwrap()[0].clone();
                    match left.try_unwrap_heap() {
                        Ok(HeapObject::List(mut vec)) => {
                            vec.push(x);
                            self.stack.push(NValue::list(vec));
                        }
                        Ok(_) => unreachable!(),
                        Err(left) => {
                            let mut result = left.as_list().unwrap().clone();
                            result.push(x);
                            self.stack.push(NValue::list(result));
                        }
                    }
                } else {
                    // General case: try CoW on left, extend with right
                    let r_items_cloned: Vec<NValue> = right.as_list().unwrap().to_vec();
                    match left.try_unwrap_heap() {
                        Ok(HeapObject::List(mut vec)) => {
                            vec.extend(r_items_cloned);
                            self.stack.push(NValue::list(vec));
                        }
                        Ok(_) => unreachable!(),
                        Err(left) => {
                            let l_items = left.as_list().unwrap();
                            let mut result =
                                Vec::with_capacity(l_items.len() + r_items_cloned.len());
                            result.extend_from_slice(l_items);
                            result.extend(r_items_cloned);
                            self.stack.push(NValue::list(result));
                        }
                    }
                }
            }
            Op::ListTailFrom(n) => {
                let list_val = self.pop_fast();
                let from = *n as usize;
                // CoW: if uniquely owned, drain the prefix in-place
                match list_val.try_unwrap_heap() {
                    Ok(HeapObject::List(mut vec)) => {
                        vec.drain(..from);
                        self.stack.push(NValue::list(vec));
                    }
                    Ok(_) => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error("ListTailFrom requires List".into(), line, col));
                    }
                    Err(list_val) => {
                        if let Some(items) = list_val.as_list() {
                            let tail: Vec<NValue> = items[from..].to_vec();
                            self.stack.push(NValue::list(tail));
                        } else {
                            let (line, col) = chunk.source_map[ip - 1];
                            return Err(self.error(
                                format!("ListTailFrom requires List, got {}", list_val),
                                line,
                                col,
                            ));
                        }
                    }
                }
            }
            Op::MakeRecord(n) => {
                let count = *n as usize;
                let start = self.stack.len() - count * 2;
                let pairs: Vec<NValue> = self.stack.drain(start..).collect();
                let mut fields = Vec::with_capacity(count);
                for pair in pairs.chunks(2) {
                    if pair[0].is_heap() {
                        if let HeapObject::String(key) = pair[0].as_heap_ref() {
                            fields.push((key.clone(), pair[1].clone()));
                        } else {
                            let (line, col) = chunk.source_map[ip - 1];
                            return Err(self.error(
                                format!("Record key must be String, got {}", pair[0]),
                                line,
                                col,
                            ));
                        }
                    } else {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("Record key must be String, got {}", pair[0]),
                            line,
                            col,
                        ));
                    }
                }
                self.stack.push(NValue::record(fields));
            }
            Op::GetField(name_idx) => {
                let field_name = match chunk.constants[*name_idx as usize].as_string() {
                    Some(s) => s.clone(),
                    None => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            "GetField constant must be String".into(),
                            line,
                            col,
                        ));
                    }
                };
                let record = self.pop_fast();
                if !record.is_heap() {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("Cannot access field '{}' on {}", field_name, record),
                        line,
                        col,
                    ));
                }
                match record.as_heap_ref() {
                    HeapObject::Record(fields) | HeapObject::Struct { fields, .. } => {
                        match fields.iter().find(|(k, _)| *k == field_name) {
                            Some((_, v)) => self.stack.push(v.clone()),
                            None => {
                                let (line, col) = chunk.source_map[ip - 1];
                                return Err(self.error(
                                    format!("Record has no field '{}'", field_name),
                                    line,
                                    col,
                                ));
                            }
                        }
                    }
                    _ => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("Cannot access field '{}' on {}", field_name, record),
                            line,
                            col,
                        ));
                    }
                }
            }
            Op::GetFieldIdx(field_idx, name_idx) => {
                let field_name = match chunk.constants[*name_idx as usize].as_string() {
                    Some(s) => s.clone(),
                    None => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            "GetFieldIdx constant must be String".into(),
                            line,
                            col,
                        ));
                    }
                };
                let record = self.pop_fast();
                if !record.is_heap() {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("Cannot access field '{}' on {}", field_name, record),
                        line,
                        col,
                    ));
                }
                match record.as_heap_ref() {
                    HeapObject::Record(fields) | HeapObject::Struct { fields, .. } => {
                        let idx = *field_idx as usize;
                        // Fast path: direct index access with name verification
                        if idx < fields.len() && *fields[idx].0 == *field_name {
                            self.stack.push(fields[idx].1.clone());
                        } else {
                            // Fallback: linear scan (layout mismatch or row polymorphism)
                            match fields.iter().find(|(k, _)| *k == field_name) {
                                Some((_, v)) => self.stack.push(v.clone()),
                                None => {
                                    let (line, col) = chunk.source_map[ip - 1];
                                    return Err(self.error(
                                        format!("Record has no field '{}'", field_name),
                                        line,
                                        col,
                                    ));
                                }
                            }
                        }
                    }
                    _ => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("Cannot access field '{}' on {}", field_name, record),
                            line,
                            col,
                        ));
                    }
                }
            }
            Op::MakeTuple(n) => {
                let count = *n as usize;
                let start = self.stack.len() - count;
                let items: Vec<NValue> = self.stack.drain(start..).collect();
                self.stack.push(NValue::tuple(items));
            }
            Op::TupleGet(idx) => {
                let tuple = self.pop_fast();
                if tuple.is_heap() {
                    if let HeapObject::Tuple(items) = tuple.as_heap_ref() {
                        let i = *idx as usize;
                        if i >= items.len() {
                            let (line, col) = chunk.source_map[ip - 1];
                            return Err(self.error(
                                format!("Tuple index {} out of bounds (len {})", i, items.len()),
                                line,
                                col,
                            ));
                        }
                        self.stack.push(items[i].clone());
                    } else {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("TupleGet requires Tuple, got {}", tuple),
                            line,
                            col,
                        ));
                    }
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("TupleGet requires Tuple, got {}", tuple),
                        line,
                        col,
                    ));
                }
            }
            Op::MakeEnum(tag_idx, tag_id) => {
                let tag = match chunk.constants[*tag_idx as usize].as_string() {
                    Some(s) => s.clone(),
                    None => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            "MakeEnum constant must be String".into(),
                            line,
                            col,
                        ));
                    }
                };
                let payload = self.pop_fast();
                self.stack
                    .push(NValue::enum_val_with_id(tag, *tag_id as u32, payload));
            }
            Op::MakeStruct(tag_idx) => {
                let tag = match chunk.constants[*tag_idx as usize].as_string() {
                    Some(s) => s.clone(),
                    None => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            "MakeStruct constant must be String".into(),
                            line,
                            col,
                        ));
                    }
                };
                let record = self.pop_fast();
                if record.is_heap() {
                    if let HeapObject::Record(fields) = record.as_heap_ref() {
                        self.stack.push(NValue::struct_val(tag, fields.clone()));
                    } else {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("MakeStruct requires Record, got {}", record),
                            line,
                            col,
                        ));
                    }
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("MakeStruct requires Record, got {}", record),
                        line,
                        col,
                    ));
                }
            }
            Op::EnumTag => {
                let val = self.pop_fast();
                if val.is_heap() {
                    if let HeapObject::Enum { tag, .. } = val.as_heap_ref() {
                        self.stack.push(NValue::string(tag.clone()));
                    } else {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("EnumTag requires Enum, got {}", val),
                            line,
                            col,
                        ));
                    }
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("EnumTag requires Enum, got {}", val),
                        line,
                        col,
                    ));
                }
            }
            Op::EnumTagId => {
                let val = self.pop_fast();
                if val.is_heap() {
                    if let HeapObject::Enum { tag_id, .. } = val.as_heap_ref() {
                        self.stack.push(NValue::int(*tag_id as i64));
                    } else {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("EnumTagId requires Enum, got {}", val),
                            line,
                            col,
                        ));
                    }
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("EnumTagId requires Enum, got {}", val),
                        line,
                        col,
                    ));
                }
            }
            Op::EnumPayload => {
                let val = self.pop_fast();
                if val.is_heap() {
                    if let HeapObject::Enum { payload, .. } = val.as_heap_ref() {
                        // Backward compat: single-element → push that element,
                        // empty → push Unit, multi → wrap as Tuple.
                        let result = match payload.len() {
                            0 => NValue::unit(),
                            1 => payload[0].clone(),
                            _ => NValue::tuple(payload.clone()),
                        };
                        self.stack.push(result);
                    } else {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("EnumPayload requires Enum, got {}", val),
                            line,
                            col,
                        ));
                    }
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("EnumPayload requires Enum, got {}", val),
                        line,
                        col,
                    ));
                }
            }
            Op::MakeEnumN(tag_idx, tag_id, arg_count) => {
                let n = *arg_count as usize;
                let tag = match chunk.constants[*tag_idx as usize].as_string() {
                    Some(s) => s.clone(),
                    None => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            "MakeEnumN constant must be String".into(),
                            line,
                            col,
                        ));
                    }
                };
                let start = self.stack.len() - n;
                let payload: Vec<NValue> = self.stack.drain(start..).collect();
                let id = if *tag_id == u16::MAX {
                    u32::MAX
                } else {
                    *tag_id as u32
                };
                self.stack.push(NValue::enum_val_flat(tag, id, payload));
            }
            Op::EnumFieldGet(idx) => {
                let val = self.pop_fast();
                if val.is_heap() {
                    if let HeapObject::Enum { payload, .. } = val.as_heap_ref() {
                        let i = *idx as usize;
                        if i < payload.len() {
                            self.stack.push(payload[i].clone());
                        } else {
                            // Empty payload (nullary or Unit-wrapped) → return Unit.
                            // This handles Ok(()) and similar cases where the ? operator
                            // extracts field 0 from an enum with Unit payload.
                            self.stack.push(NValue::unit());
                        }
                    } else {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("EnumFieldGet requires Enum, got {}", val),
                            line,
                            col,
                        ));
                    }
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("EnumFieldGet requires Enum, got {}", val),
                        line,
                        col,
                    ));
                }
            }
            Op::EnumFieldSet(idx) => {
                // Stack: [new_value, enum] → [updated_enum]
                let mut enum_val = self.pop_fast();
                let new_value = self.pop_fast();
                let i = *idx as usize;
                if let Some(heap) = enum_val.as_heap_mut() {
                    // Sole owner — update field in place
                    if let HeapObject::Enum { payload, .. } = heap {
                        if i < payload.len() {
                            payload[i] = new_value;
                        }
                    }
                    self.stack.push(enum_val);
                } else if enum_val.is_heap() {
                    // Shared — clone payload and construct new enum
                    if let HeapObject::Enum {
                        tag,
                        tag_id,
                        payload,
                    } = enum_val.as_heap_ref()
                    {
                        let mut new_payload = payload.clone();
                        if i < new_payload.len() {
                            new_payload[i] = new_value;
                        }
                        self.stack.push(NValue::enum_val_flat(
                            tag.clone(),
                            *tag_id,
                            new_payload,
                        ));
                    } else {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("EnumFieldSet requires Enum, got {}", enum_val),
                            line,
                            col,
                        ));
                    }
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("EnumFieldSet requires Enum, got {}", enum_val),
                        line,
                        col,
                    ));
                }
            }
            Op::UpdateRecord(n) => {
                let count = *n as usize;
                let start = self.stack.len() - count * 2;
                let updates: Vec<NValue> = self.stack.drain(start..).collect();
                let base = self.pop_fast();
                if !base.is_heap() {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("UpdateRecord requires Record, got {}", base),
                        line,
                        col,
                    ));
                }
                match base.as_heap_ref() {
                    HeapObject::Record(fields) => {
                        let mut new_fields = fields.clone();
                        for pair in updates.chunks(2) {
                            if pair[0].is_heap()
                                && let HeapObject::String(key) = pair[0].as_heap_ref()
                            {
                                if let Some(existing) =
                                    new_fields.iter_mut().find(|(k, _)| *k == *key)
                                {
                                    existing.1 = pair[1].clone();
                                } else {
                                    // Records are open: append new field
                                    new_fields.push((key.clone(), pair[1].clone()));
                                }
                            }
                        }
                        self.stack.push(NValue::record(new_fields));
                    }
                    HeapObject::Struct { name, fields } => {
                        let mut new_fields = fields.clone();
                        for pair in updates.chunks(2) {
                            if pair[0].is_heap()
                                && let HeapObject::String(key) = pair[0].as_heap_ref()
                            {
                                if let Some(existing) =
                                    new_fields.iter_mut().find(|(k, _)| *k == *key)
                                {
                                    existing.1 = pair[1].clone();
                                } else {
                                    let (line, col) = chunk.source_map[ip - 1];
                                    return Err(self.error(
                                        format!("Record has no field '{}'", key),
                                        line,
                                        col,
                                    ));
                                }
                            }
                        }
                        self.stack
                            .push(NValue::struct_val(name.clone(), new_fields));
                    }
                    _ => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("UpdateRecord requires Record, got {}", base),
                            line,
                            col,
                        ));
                    }
                }
            }
            _ => unreachable!("dispatch_data_structures called with non-data-structure op"),
        }
        Ok(DispatchResult::Continue)
    }
}
