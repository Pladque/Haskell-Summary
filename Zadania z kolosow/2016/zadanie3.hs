

data Rd a = Empty | Leaf a | Node a [Rd a] 

unpackRd Empty = []
unpackRd (Leaf x) = [x]
unpackRd (Node x xs) =  (x : (concat (map unpackRd xs)) )

-- el rd item = 

rd = Node 5 [(Leaf 2), (Node 19 [Leaf 1])]
rd2 = Leaf 5

el rd item = item `elem` (unpackRd rd)


-------

subst x newX rd = (substHelp x newX rd )

substHelp x newX (Leaf a) | a==x=(Leaf newX) | otherwise=(Leaf a)
substHelp x newX (Node a xs)  | a==x=(Node newX (map (substHelp x newX ) xs)) | otherwise= (Node a (map (substHelp x newX ) xs))


rd2list = unpackRd
