data Mb a = Drzewo ([a]) ([a])


dnp (Drzewo l r) newElem = (Drzewo (newElem:l) r)
dnk (Drzewo l r) newElem = (Drzewo l (r ++ [newElem]))


mb2list (Drzewo l r) = l ++ r

d1 = (Drzewo [1,2,3,4] [5,6,7,8])

ull (Drzewo l r) = (Drzewo [] r)
upl (Drzewo l r) = (Drzewo l [])
