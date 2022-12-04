

getReplacement x [] = x
getReplacement x pairs | x== (fst (head pairs))=(snd(head pairs)) | otherwise=do
    getReplacement x (tail pairs)


go pairs x previousResult = [ (getReplacement x pairs):pref | pref <- previousResult]

repl xs pairs = last (foldr (go pairs) [[]] xs)


-- repl xs pair = [x | x <- xs]