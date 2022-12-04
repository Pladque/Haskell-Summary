-- niedokocznoen
type Point = (Double, Double)

dist p1 p2 = sqrt ((fst p1 - fst p2)**2 + (snd p1 - snd p2)**2)

distanceLists ind p xs = minimum (map (\x-> snd x) (filter (\x->ind /= fst x)  (zip [0..] (map (dist p) xs))))

-- minDist :: [Point] -> (Point, Point, Double)
minDist xs = [((snd x), distanceLists (fst x) (snd x) xs) | x <- zip [0..] xs  ]
