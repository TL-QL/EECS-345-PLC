{- EECS 345 Programming Exercise -}
{- Qiwen Luo qxl216-}

{- rotate -}
{- takes three elements a1, a2, a3 and a list lis-}
{- returns a list that is the same as the input list except that each occurrence of the first element is replaced by the second, the second element is replaced by the third, and the third is replaced by the first.-}
rotate _ _ _ [] = []
rotate a1 a2 a3 (h : t)
 | h == a1 = a2 : (rotate a1 a2 a3 t)
 | h == a2 = a3 : (rotate a1 a2 a3 t)
 | h == a3 = a1 : (rotate a1 a2 a3 t)
 | otherwise = h : (rotate a1 a2 a3 t)

{- squareroot -}
{- takes two numbers, a value v and an iteration iter -}
{- return the squareroot of the value v-}
squareroot v 0 = v
squareroot v iter = (squareroot v (iter - 1))
                    - (((squareroot v (iter - 1)) * (squareroot v (iter - 1))) - v) 
                    / (2 * (squareroot v (iter - 1)))

{- squareroot_cps -}
{- cps version for squareroot -}
squareroot_cps v 0 return = return v
squareroot_cps v iter return = squareroot_cps v (iter - 1) (\v1 -> return (v1 - (v1 * v1 - v) / (2 * v1)))

{- MyList -}
{- A type that allows us to have nested lists-}
data MyList t = Element t | SubList [MyList t]  deriving (Show, Eq)

{- grotate -}
{- takes three values a1, a2, a3 and list lis containing elements and sublists-}
{- returns a list with the same structure, but if any "element" is the first input, it is replaced by the second, if an "element" is the second input, it is replaced by the third, and if it is the "third" input, it is replaced by the first -}
grotate a1 a2 a3 [] = []
grotate a1 a2 a3 ((SubList h) : t) = (SubList (grotate a1 a2 a3 h)) : (grotate a1 a2 a3 t)
grotate a1 a2 a3 ((Element h) : t)
 | h == a1 = (Element a2) : (grotate a1 a2 a3 t)
 | h == a2 = (Element a3) : (grotate a1 a2 a3 t)
 | h == a3 = (Element a1) : (grotate a1 a2 a3 t)
 | otherwise = (Element h) : (grotate a1 a2 a3 t)

 {- Given a Haskell type for a Binary tree that stores a value at every node -}
data BinaryTree t = Empty | Leaf t | InnerNode t (BinaryTree t) (BinaryTree t) deriving (Show, Eq)

{- removeMin -}
{- takes a Tree as input -}
{- return a new tree with the smallest value of the tree removed -}
removeMin Empty                        = Empty
removeMin (Leaf a)                     = Empty
removeMin (InnerNode b Empty (Leaf r)) = Leaf r
removeMin (InnerNode b (Leaf l) Empty) = Leaf b
removeMin (InnerNode b Empty r)        = InnerNode (findMin r) Empty (removeMin r)
removeMin (InnerNode b l r)            = InnerNode b (removeMin l) r

{- helper function for removeMin -}
{- findMin -}
{- takes part of a Tree as input -}
{- return the smallest value of right child -}
findMin (InnerNode b Empty r)    = b
findMin (InnerNode b (Leaf a) r) = a
findMin (InnerNode b l r)        = findMin l

{- dotproduct -}
{- takes two vectors v1 v2 -}
{- returns the dot product of the vectors -}
{- if they do not have the same length, it should return Nothing -}
dotproduct [] []               = (Just 0)
dotproduct [] v2               = Nothing
dotproduct v1 []               = Nothing
dotproduct (h1 : t1) (h2 : t2) = do
    y <- (dotproduct t1 t2)
    return (h1 * h2 + y)

{- vectormult -}
{- takes a row vector v and matrix m -}
{- return the multiplication of v and m or Nothing if format not match -}
vectormult v [] = (Just [])
vectormult v m 
 | isEmpty m = (Just [])
 | otherwise = do
    a <- (getcol m)
    b <- (dotproduct v a)
    c <- (vectormult v (map tail m))
    return (b : c)

{- helper function for vectormult -}
{- getcol -}
{- takes m [(h : t)]-}
{- return the first column of m -}
getcol []       = (Just [])
getcol ([] : t) = do
    y <- (getcol t)
    return ([] ++ y )
getcol (h : t)  = do
    y <- (getcol t)
    return ((head h) : y )

{- helper function for vectormult -}
{- isEmpty -}
{- takes a matrix m [(h : t)] -}
{- return True if m contains only []s else return False -}
isEmpty []   = True
isEmpty (h : t)
 | h == []   = isEmpty t
 | otherwise = False

{- matrixmultiply-} 
{- takes two matrices m1 m2 -}
{- return the multiplication of m1 and m2 or return Nothing if format not match -}
matrixmultiply [] _       = (Just [])
matrixmultiply (h : t) m2 = do
 x <- (vectormult h m2)
 y <- (matrixmultiply t m2)
 return (x : y)

{- Create a list monad that generalizes a list -}
data MList t = Pair t (MList t) | Null deriving (Show, Eq)

{- lreturn to make a list monad -}
lreturn x = Pair x (Null)

{- lbind create a binding function -}
lbind:: MList t -> (t -> MList t -> MList t) -> MList t
lbind (Pair x y) f = f x (lbind y f)
lbind Null _       = Null
