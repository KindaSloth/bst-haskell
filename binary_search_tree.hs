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

delete :: BST -> Int -> BST
delete Null _ = Null
delete (Node left value right) x
    | x > value = Node left value (delete right x)
    | x < value = Node (delete left x) value right
    | x == value = deleteNode (Node left value right)
    | otherwise = error "number already exist in the tree"

deleteNode :: BST -> BST
deleteNode (Node Null value right) = right
deleteNode (Node left value Null) = left
deleteNode (Node left value right) = insert left (getValue right)

getValue :: BST -> Int
getValue (Node _ value _) = value

is_empty :: BST -> Bool
is_empty Null = True
is_empty (Node _ value _) = False