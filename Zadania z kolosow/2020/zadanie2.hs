data Tree a = Empty | Node a (Tree a) (Tree a)

findPath :: Eq a => a -> Tree a -> [a]

findPathHelper dest Empty path = []
findPathHelper dest (Node val l r) path | val==dest=val:path |otherwise=do
    ((findPathHelper dest l (val:path)) ++ (findPathHelper dest r (val:path)))

findPath dest tree = reverse (findPathHelper dest tree [])

t=Node 10 (Node 5 (Node 4 Empty Empty) (Node 6 Empty Empty)) (Node 20 Empty Empty)
