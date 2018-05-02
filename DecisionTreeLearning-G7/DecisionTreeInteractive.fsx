type Tree<'a> = 
    | Empty
    | Node of value: 'a * left: Tree<'a> * right: Tree<'a>

type DecisionTree<'a> = 
    | Empty
    | LeafNode of value: string
    | InnerNode of value: string * Tree2<'a> list

