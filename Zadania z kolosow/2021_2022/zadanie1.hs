type DirectedGraph = ([Int], Int -> Int -> Bool)

atDistance :: DirectedGraph -> Int -> Int -> [Int]

-- g=graf
-- v=poczatkowy wierzcholek
-- d=dlugosc drogi
atDistance g 0 v = [v]
atDistance g d v = concat [ atDistance g (d-1) x | x <- (fst g) , (snd g) v x ]

-- atDistance2 :: DirectedGraph -> Int -> Int -> [Int]
-- atDistance2 (vs, g) d v
--       | d <= 0 = []
--       | d == 1 = filter (\x -> g v x) vs
--       | otherwise = foldl (++) [] (map (\y -> atDistance2 (vs, g) (d-1) y) (filter (\x -> g v x) vs))


g :: DirectedGraph
g = ([1,2,3,4,5], isNeigh)

 

isNeigh :: Int -> Int -> Bool
isNeigh 1 2 = True
isNeigh 2 1 = True
isNeigh 2 3 = True
isNeigh 2 4 = True
isNeigh 3 5 = True
isNeigh x y = False