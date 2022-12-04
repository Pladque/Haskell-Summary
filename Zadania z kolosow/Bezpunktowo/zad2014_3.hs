-- racezej to tez zle BO MAM TA LAMBDE ZAJEBANA

h :: [a] -> [a]
-- h xs = map (\tuple->snd tuple) (filter (\tuple->fst tuple `mod` 2 == 0) (zip [0..] xs))
-- h xs= ((map (\tuple->snd tuple)).(filter (\tuple->fst tuple `mod` 2 == 0))) (zip [0..] xs)
h  = ((map (\tuple->snd tuple)).(filter (\tuple->fst tuple `mod` 2 == 0))).(zip [0..])
