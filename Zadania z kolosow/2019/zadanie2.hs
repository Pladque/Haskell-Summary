toBinary :: Int -> [ Int ]

toBinary 0 = [ 0 ]

toBinary n = toBinary ( n `quot` 2 ) ++ [ n `rem` 2 ]

toBinaryFixed 0 = [ 0 ]
toBinaryFixed n = tail (toBinary n)


addLeadingZeros n xs = [0 | x<-[1..(-(length xs) + n)]] ++ xs

isPalindrome w = w == reverse w

allBinsWithLength n = [ addLeadingZeros n (toBinaryFixed x) | x <-[0..((2^(n)-1))] ]

bpHelp n  | (n `mod` 2 ==1 )=[] | otherwise=do 
    filter (\x-> ((sum x) == (div n 2))&& (isPalindrome x) ) (allBinsWithLength n)

toAB x | (x==0)='a' | otherwise='b'

bp n = map (map (\x -> toAB x)) (bpHelp n)