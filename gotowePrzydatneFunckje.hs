-- zamiana listy na stringa

-- zamiana boola na inta

-- pre/in/postorderowe wyszukiwanie w drzewie

-- fibbonaci
fibonacci :: [Integer]
fibonacci = 0 : 1 : (zipWith (+)fibonacci (tail fibonacci))

-- sortowanie (dla typu majacego Ord)
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort (x:xs) = insert $ insertionSort xs
    where insert [] = [x]
          insert (y:ys)
              | x < y = x : y : ys
              | otherwise = y : insert ys


-- sortowanie (dla typu majacego customowa funckja porownujaca)


-- toUpper / toLower

-- isDigid / isNumber