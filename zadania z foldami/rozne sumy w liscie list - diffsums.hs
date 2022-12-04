

sumAll xs = map sum xs

go x previousResult = [ x:pref | pref <- previousResult, elem (sum x) (sumAll pref) == False  ] ++ [ pref | pref <- previousResult, elem (sum x) (sumAll pref) == True  ]


diffsums xs =  foldr go [[]] xs

