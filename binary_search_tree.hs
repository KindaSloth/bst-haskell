data BST = Null | Node BST Int BST 
    deriving(Show)

insert :: BST -> Int -> BST
insert Null x = (Node Null x Null)
insert (Node left value right) x
    | x > value = Node left value (insert right x)
    | x < value = Node (insert left x) value right
    | otherwise = error "number already exist in the tree"

search :: BST -> Int -> BST
search Null x = error "Empty tree"
search (Node left value right) x
    | x > value = search right x
    | x < value = search left x
    | x == value = (Node left value right)
    | otherwise = error "this numder don't exist in the tree yet"