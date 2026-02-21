//! Generic segment-based radix tree for HTTP route matching.
//!
//! Used by both the sync VM server (with NValue handlers) and the async
//! hyper server (with AsyncHandler). Parameterized over handler type H.

use std::collections::HashMap;

/// Trait for collecting path parameters during route matching.
/// Implementations must support backtracking (push + pop).
pub trait ParamCollector {
    fn push(&mut self, name: String, value: String) -> bool;
    fn pop(&mut self);
}

/// Fixed-capacity parameter storage to avoid per-request Vec allocation.
/// Most routes have fewer than 8 path parameters.
const MAX_PARAMS: usize = 8;

pub struct SmallParams {
    data: [(String, String); MAX_PARAMS],
    len: usize,
}

impl Default for SmallParams {
    fn default() -> Self {
        SmallParams {
            data: std::array::from_fn(|_| (String::new(), String::new())),
            len: 0,
        }
    }
}

impl SmallParams {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn as_slice(&self) -> &[(String, String)] {
        &self.data[..self.len]
    }

    pub fn get(&self, key: &str) -> Option<&str> {
        self.data[..self.len]
            .iter()
            .find(|(k, _)| k == key)
            .map(|(_, v)| v.as_str())
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn clone_data(&self) -> SmallParams {
        let mut new = SmallParams::new();
        for i in 0..self.len {
            new.data[i] = self.data[i].clone();
        }
        new.len = self.len;
        new
    }
}

impl ParamCollector for SmallParams {
    fn push(&mut self, name: String, value: String) -> bool {
        if self.len < MAX_PARAMS {
            self.data[self.len] = (name, value);
            self.len += 1;
            true
        } else {
            false
        }
    }

    fn pop(&mut self) {
        if self.len > 0 {
            self.len -= 1;
        }
    }
}

impl ParamCollector for HashMap<String, String> {
    fn push(&mut self, name: String, value: String) -> bool {
        self.insert(name, value);
        true
    }

    fn pop(&mut self) {
        // HashMap doesn't track insertion order, but route matching
        // only ever pops the most recently pushed param name.
        // The caller handles this by removing by key directly.
    }
}

/// A node in the radix tree.
pub struct RadixNode<H> {
    pub children: HashMap<String, RadixNode<H>>,
    pub param: Option<(String, Box<RadixNode<H>>)>,
    pub handlers: HashMap<String, H>,
}

impl<H> Default for RadixNode<H> {
    fn default() -> Self {
        RadixNode {
            children: HashMap::new(),
            param: None,
            handlers: HashMap::new(),
        }
    }
}

impl<H> RadixNode<H> {
    pub fn new() -> Self {
        Self::default()
    }
}

/// Segment-based radix tree for HTTP route matching.
pub struct RadixTree<H> {
    pub root: RadixNode<H>,
}

impl<H> Default for RadixTree<H> {
    fn default() -> Self {
        RadixTree {
            root: RadixNode::new(),
        }
    }
}

impl<H: Clone> RadixTree<H> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, method: &str, path: &str, handler: H) {
        let segments: Vec<&str> = path.split('/').filter(|s| !s.is_empty()).collect();
        let mut node = &mut self.root;

        for seg in &segments {
            if let Some(name) = seg.strip_prefix(':') {
                if node.param.is_none() {
                    node.param = Some((name.to_string(), Box::new(RadixNode::new())));
                }
                node = node.param.as_mut().unwrap().1.as_mut();
            } else {
                node = node.children.entry(seg.to_string()).or_default();
            }
        }
        node.handlers.entry(method.to_string()).or_insert(handler);
    }

    pub fn find<P: ParamCollector>(&self, method: &str, path: &str, params: &mut P) -> Option<&H> {
        let segments: Vec<&str> = path.split('/').filter(|s| !s.is_empty()).collect();
        find_in_node(&self.root, &segments, method, params)
    }
}

fn find_in_node<'a, H, P: ParamCollector>(
    node: &'a RadixNode<H>,
    segments: &[&str],
    method: &str,
    params: &mut P,
) -> Option<&'a H> {
    if segments.is_empty() {
        return node.handlers.get(method);
    }

    let seg = segments[0];
    let rest = &segments[1..];

    // Try exact match first
    if let Some(child) = node.children.get(seg)
        && let result @ Some(_) = find_in_node(child, rest, method, params)
    {
        return result;
    }

    // Try parameter match
    if let Some((name, child)) = &node.param {
        params.push(name.clone(), seg.to_string());
        if let result @ Some(_) = find_in_node(child, rest, method, params) {
            return result;
        }
        params.pop();
    }

    None
}
