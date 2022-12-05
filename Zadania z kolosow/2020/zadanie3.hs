


go x y = x ++ [max (last x) y]

-- liczby musza byc dodatnie
sumlistMaxes xs = tail (foldl (go) ([-1]) xs)

cp xs = [(fst ind_val) + 1 | ind_val <-( zip [0..] (sumlistMaxes xs) ), (fst ind_val)+1>=(snd ind_val)]