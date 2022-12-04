-- a)
valHelper a b k | ((a `mod` b^k) == 0) = (valHelper a b (k+1)) | otherwise = (k-1)

val a b = valHelper a b 1

-- b)

g k v =  map (\x-> fst x) (filter (\x -> (snd x==v)) (zip [2..] ((map (flip val k) [2..] ))))