

go::(Eq a) => [(Integer, a)]->a->[(Integer, a)]
go prev new | (snd (last prev)) == new = ((init prev) ++ [(fst (last prev)+1, (snd (last prev)))] ) |otherwise=do
    prev ++ [(1, new)]


longr::(Eq a, Ord a) =>[a] -> (Integer, a)
longr xs =  maximum (foldl go [(0, head xs)] xs)

