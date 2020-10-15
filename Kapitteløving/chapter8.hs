
-- Chapter 8 Declaring Types and Classes

--- 8.1

data Nat = Zero | Succ Nat deriving (Show, Read, Eq, Ord)
add :: Nat -> Nat -> Nat
add Zero n      = n 
add (Succ m) n  = Succ (add m n)

mult :: Nat -> Nat -> Nat 
mult m Zero     = Zero
mult m (Succ n) = add m (mult m n)

-- 8.2

data Ordering = LT | EQ | GT
{-compare :: Ord a => a -> a -> Ordering
compare a m | a > m     = GT
            | a < m     = LT
            | a == m    = EQ-}

data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)       = x == y
occurs x (Node l y r)   =   case compare x y of 
                            LG -> occurs x l
                            EQ -> True
                            GT -> occurs x r


