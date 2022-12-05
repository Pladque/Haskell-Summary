-- zamiana listy na stringa
listtoStrHelper [] sep = ""
listtoStrHelper (x:xs) sep = ((show x) ++ sep ++ (listtoStrHelper xs sep))

--to remove last separator
listtoStr list sep = do
    let resultStr= (listtoStrHelper list sep)
    take (length (resultStr) - (length sep)) resultStr
    
    
    
-- zamiana boola na inta
boolToInt True = 1
boolToInt False = 0

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




-- toUpper / toLower

-- isDigid / isNumber

-- zamiana liczby dziesietnej na binarna
toBinary :: Int -> [ Int ]

toBinary 0 = [ 0 ]

toBinary n = toBinary ( n `quot` 2 ) ++ [ n `rem` 2 ]

toBinaryFixed 0 = [ 0 ]
toBinaryFixed n = tail (toBinary n)

-- grupowanie elementow listy po n elelmntow
group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative or zero n"
  
  
-- dodawanie elementow (tu 0) na poczatek az do uzyskania dlugosci n
addLeadingZeros n xs = [0 | x<-[1..(-(length xs) + n)]] ++ xs


-- wstawianie elelmtnu do listy n n-te miesce
insertAt :: [a] -> a -> Int -> [a]
insertAt [] elem pos = [elem]
insertAt (x:xs) elem pos
    | pos == 0 = elem : x : xs
    | pos > 0 = x : insertAt xs elem (pos - 1) 
    | otherwise = x : insertAt xs elem ((pos) + length (x:xs) )
    
    
-- Sprawdzanie czy Palindrom
isPalindrome w = w == reverse w

-- maxy w podlistach
-- liczby musza byc nieujemne
sumlistMaxes xs = tail (foldl (go) ([-1]) xs)

-- generowanie n liczb pierwszych
primes = sieve [2..]  
sieve (x : xs) = x : sieve [y | y <- xs, y `mod` x > 0] 

-- split listy po separatorze
splitList ::(Eq a) => [a] -> a -> [[a]]
splitList [] sep = [[]]
splitList xs sep = do
    (takeWhile (\x -> (x /= sep)) xs)  : (splitList (tail (dropWhile (\x -> x /= sep) xs)) sep)


splitListFixed xs sep = init (splitList (xs ++ [sep]) sep)



-- usuwanie niechcianych znakow ze stringa
--	(dziala tez dla list normlanie po usunieciu lini z deklaracja typu
ugly = ['.', ',','!', '?']

removeUglyCharacters :: String-> [Char] -> String
removeUglyCharacters [] ugly = []
removeUglyCharacters (x:xs) ugly | x `elem` ugly  = (removeUglyCharacters xs ugly)
                            | otherwise = x:(removeUglyCharacters xs ugly)
