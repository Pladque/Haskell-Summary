
areEqual :: (Eq a) =>(a,a)->Bool
areEqual tuple = fst tuple == snd tuple

-- zle bo mam lambde ktora przyjmuje argument a nie pwo

compref :: (Eq a) => [a] -> [a]-> Int
-- compref xs1 xs2 = length (takeWhile (\x->x) (map areEqual (zip xs1 xs2)))
-- compref xs1 xs2 = ((length).(takeWhile (\x->x))) (map areEqual (zip xs1 xs2)) 
-- compref xs1 xs2 = (((length).(takeWhile (\x->x))).(map areEqual)) (zip xs1 xs2) 
-- compref xs1 xs2 = ((((length).(takeWhile (\x->x))).(map areEqual)).(zip xs1)) xs2 
-- compref xs1 =      (((         (length).(takeWhile (\x->x))).(map areEqual)).(zip xs1))
-- compref xs1 =      (((     (.) (length)(takeWhile (\x->x))).(map areEqual)).(zip xs1))
-- compref xs1 =      (((.) ( (.) (length)(takeWhile (\x->x))) (map areEqual)).(zip xs1))
-- compref xs1 = ((.) ( (.) ( (.) (length)(takeWhile (\x->x))) (map areEqual)).zip) xs1
compref =  (.) ((.) ( (.) length (takeWhile (\x->x))) (map areEqual)).zip


-- compref =  (.) ((.) ( (.) length (takeWhile (\x->x))) (map areEqual)).zip



