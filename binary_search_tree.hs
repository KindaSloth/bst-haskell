import Data.Semigroup
import Data.Monoid
import Data.Foldable
import Data.Functor

data BST a = Null | Node (BST a) a (BST a)
    deriving(Show, Ord, Eq)

instance Foldable BST where
    foldMap f (Node Null x Null) = f x 
    foldMap f (Node left x Null) = foldMap f left <> f x
    foldMap f (Node Null x right) = f x <> foldMap f right
    foldMap f (Node left x right) = foldMap f left <> f x <> foldMap f right

instance Functor BST where
    fmap f (Node Null x Null) = Node Null (f x) Null
    fmap f (Node left x Null) = Node (fmap f left) (f x) Null
    fmap f (Node Null x right) = Node Null (f x) (fmap f right)
    fmap f (Node left x right) = Node (fmap f left) (f x) (fmap f right)

insert :: BST Int -> Int -> BST Int
insert Null x = Node Null x Null
insert (Node left value right) x
    | x > value = Node left value (insert right x)
    | x < value = Node (insert left x) value right
    | otherwise = error "number already exist in the tree"

search :: BST Int -> Int -> BST Int
search Null x = error "Empty tree"
search (Node left value right) x
    | x > value = search right x
    | x < value = search left x
    | x == value = Node left value right
    | otherwise = error "this numder don't exist in the tree yet"

delete :: BST Int -> Int -> BST Int
delete Null _ = Null
delete (Node left value right) x
    | x > value = Node left value (delete right x)
    | x < value = Node (delete left x) value right
    | x == value = deleteNode (Node left value right)
    | otherwise = error "number already exist in the tree"

deleteNode :: BST Int -> BST Int
deleteNode (Node Null value right) = right
deleteNode (Node left value Null) = left
deleteNode (Node left value right) = insertNode left right

insertNode :: BST Int -> BST Int -> BST Int
insertNode (Node left value Null) rightNode = Node left value rightNode
insertNode (Node left value right) rightNode = Node left value (insertNode right rightNode)

is_empty :: BST Int -> Bool
is_empty Null = True
is_empty (Node _ value _) = False