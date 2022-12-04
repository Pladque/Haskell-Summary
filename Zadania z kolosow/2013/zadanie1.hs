toBinary :: Int -> [ Int ]

toBinary 0 = [ 0 ]

toBinary n = toBinary ( n `quot` 2 ) ++ [ n `rem` 2 ]

toBinaryFixed 0 = [ 0 ]
toBinaryFixed n = tail (toBinary n)

oddbinsHelper 1 = [1]
oddbinsHelper n = ( 0 : (oddbinsHelper (n-1))) ++  (1 : (oddbinsHelper (n-1)))

allBinary n = [toBinaryFixed x |x<- [1..n]] 

addLeadingZeros n xs = [0 | x<-[1..(-(length xs) + n)]] ++ xs

oddbins n = map (addLeadingZeros n) (filter (\x -> (sum x) `mod` 2 == 1) (allBinary (2^n - 1)))