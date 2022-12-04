

shiftByOne (x:xs) = xs ++ [x] 

shiftBy xs 0 = xs
shiftBy xs k = (shiftBy (shiftByOne xs) (k-1))

doCycle xs = foldl (\x y -> x ++ [shiftBy xs y]) [] [0..((length xs)-1)] 
