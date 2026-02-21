// Binary search tree: insert 100000 keys, sum all values
// Tests: allocation pressure (Box), enum-based data structure, recursion

enum Tree {
    Leaf,
    Node(i64, i64, Box<Tree>, Box<Tree>),
}

fn insert(t: Tree, key: i64, val: i64) -> Tree {
    match t {
        Tree::Leaf => Tree::Node(key, val, Box::new(Tree::Leaf), Box::new(Tree::Leaf)),
        Tree::Node(k, v, left, right) => {
            if key < k {
                Tree::Node(k, v, Box::new(insert(*left, key, val)), right)
            } else if key > k {
                Tree::Node(k, v, left, Box::new(insert(*right, key, val)))
            } else {
                Tree::Node(k, val, left, right)
            }
        }
    }
}

fn tree_sum(t: &Tree) -> i64 {
    match t {
        Tree::Leaf => 0,
        Tree::Node(_, v, left, right) => v + tree_sum(left) + tree_sum(right),
    }
}

fn main() {
    let mut root = Tree::Leaf;
    let mut seed: i64 = 42;
    for _ in 0..100000 {
        seed = seed.wrapping_mul(1103515245).wrapping_add(12345);
        if seed < 0 { seed = -seed; }
        seed %= 1000000007;
        let key = seed % 100000;
        root = insert(root, key, key);
    }
    println!("{}", tree_sum(&root));
}
