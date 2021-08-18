import Data.Semigroup
import Data.Monoid
import Data.Foldable
import Data.Functor
import Control.Applicative

data BST a = Null | Node (BST a) a (BST a)
    deriving(Show, Ord, Eq)

instance Ord a => Semigroup (BST a) where
    Null <> n = n
    n <> Null = n
    (Node left x right) <> (Node left2 y right2)
        | x > y =  (Node ((Node left2 y right2) <> left) x right)
        | x < y = (Node ((Node left x right) <> left2) y right2)
        | otherwise = (Node (left <> left2) x (right <> right2))

instance Ord a => Monoid (BST a) where
    mempty = Null

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

instance Applicative BST where
    pure x = Node Null x Null
    Null <*> _ = Null
    _ <*> Null = Null
    (Node left f right) <*> (Node l x r) = Node (left <*> l) (f x) (right <*> r)

insert :: Ord a => BST a -> a -> BST a
insert Null x = Node Null x Null
insert (Node left value right) x
    | x > value = Node left value (insert right x)
    | x < value = Node (insert left x) value right
    | otherwise = error "number already exist in the tree"

search :: Ord a => BST a -> a -> BST a
search Null x = error "Empty tree"
search (Node left value right) x
    | x > value = search right x
    | x < value = search left x
    | x == value = Node left value right
    | otherwise = error "this number don't exist in the tree yet"

delete :: Ord a => BST a -> a -> BST a
delete Null _ = Null
delete (Node left value right) x
    | x > value = Node left value (delete right x)
    | x < value = Node (delete left x) value right
    | x == value = left <> right
    | otherwise = error "this number don't exist in the tree yet"

is_empty :: BST a -> Bool
is_empty Null = True
is_empty (Node _ value _) = False
