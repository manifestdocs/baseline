use tree_sitter::Parser;

fn main() {
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_baseline::LANGUAGE.into()).unwrap();
    let source = r#"
type Tree =
  | Leaf(Int)
  | Node(Tree, Tree)

fn tree_sum(t: Tree) -> Int = {
  tree_sum(Node(Leaf(10), Leaf(20)))
}
    "#;
    let tree = parser.parse(source, None).unwrap();
    println!("{}", tree.root_node().to_sexp());
}
