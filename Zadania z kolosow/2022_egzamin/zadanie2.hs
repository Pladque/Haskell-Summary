

rpoz l1 l2 = map (\x -> (fst x, fst (snd x))) (filter (\x -> (fst (snd x)) == (snd (snd x)) ) (zip [1..] (zip l1 l2)))