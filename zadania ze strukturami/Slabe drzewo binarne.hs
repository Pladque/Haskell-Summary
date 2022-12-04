
data Sdb a = Leaf a | Node a [Sdb a]

unpackSdb (Leaf x) = [x]
unpackSdb (Node x xs) =  (x : (concat (map unpackSdb xs)) )


sdb = Node 5 [(Leaf 2), (Node 19 [Leaf 1])]
sdb2 = Leaf 5

el sdb item = item `elem` (unpackSdb sdb)

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort (x:xs) = insert $ insertionSort xs
    where insert [] = [x]
          insert (y:ys)
              | x < y = x : y : ys
              | otherwise = y : insert ys



sdb2list = unpackSdb



eq sdb1 sdb2 = (insertionSort (sdb2list sdb1) == insertionSort (sdb2list sdb2))