usunDuplikaty [] = []
usunDuplikaty (x:xs) | elem x xs = usunDuplikaty xs | otherwise= x : (usunDuplikaty xs)

boolToInt True = 1
boolToInt False = 0

count k list = sum (map (\x-> boolToInt (x==k)) list)

wIluListach :: Int -> [[Int]] -> [Int]
wIluListach n lists = [count x (concat (map usunDuplikaty lists)) | x <-[1..n]]