-- ab -> a
-- ba -> b
-- bb -> a
-- aa -> aaa

-- group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative or zero n"

replacePartOfSlowo part | part == "ab"="a" |  part == "ba"="b" |  part == "bb"="a" |  part == "aa"="aaa" | otherwise=part

czySlowoValid slowo | (length slowo) <= 1 = True | (('b' `elem` slowo) == False) = True | otherwise=False



dlugoscHelper slowo k |  (czySlowoValid (concat (map replacePartOfSlowo (group 2 slowo))) == False) =( dlugoscHelper (concat (map replacePartOfSlowo (group 2 slowo))) k+1) | otherwise=k


dlugosc slowo = dlugoscHelper slowo 1


