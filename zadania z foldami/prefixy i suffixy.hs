-- 

go x previousResult = [] : [ x:pref | pref <- previousResult]

prefixFold :: [a] -> [[a]]
prefixFold xs = foldr go [[]] xs


go2 previousResult x = [] : [ pref ++ [x] | pref <- previousResult]

suffixFold :: [a] -> [[a]]
suffixFold xs = foldl go2 [[]] xs


ps xs = (tail (prefixFold (init xs))) ++ [xs] ++ (init (reverse (suffixFold (tail xs))))