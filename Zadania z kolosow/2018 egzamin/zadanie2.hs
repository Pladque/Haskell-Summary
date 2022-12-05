-- slownik::[String] -> [(String, Int)]

splitList ::(Eq a) => [a] -> a -> [[a]]
splitList [] sep = [[]]
splitList xs sep = do
    (takeWhile (\x -> (x /= sep)) xs)  : (splitList (tail (dropWhile (\x -> x /= sep) xs)) sep)


insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort (x:xs) = insert $ insertionSort xs
    where insert [] = [x]
          insert (y:ys)
              | x < y = x : y : ys
              | otherwise = y : insert ys


splitListFixed xs sep = init (splitList (xs ++ [sep]) sep)

ugly = ['.', ',','!', '?']

removeUglyCharacters :: String-> [Char] -> String
removeUglyCharacters [] ugly = []
removeUglyCharacters (x:xs) ugly | x `elem` ugly  = (removeUglyCharacters xs ugly)
                            | otherwise = x:(removeUglyCharacters xs ugly)


go::(Eq a) => [(Integer, a)]->a->[(Integer, a)]
go prev new | (snd (last prev)) == new = ((init prev) ++ [(fst (last prev)+1, (snd (last prev)))] ) |otherwise=do
    prev ++ [(1, new)]

-- jakis IF dla sentance krotszego od 1
slownik sentence =  do
    let fixedSentence = (removeUglyCharacters sentence ugly)
    let newSentance = (insertionSort (splitListFixed fixedSentence ' '))
    reverse (insertionSort (foldl go  [(0, head newSentance)] newSentance))