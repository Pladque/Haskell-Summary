-- numocc 1 [[1,2,1,0,3],[0,2,3],[1,8,8,1,2,1]]
boolToInt True = 1
boolToInt False = 0

numocc :: (Num b, Eq a) => a -> [[a]] -> [b]
-- numocc k lists = map (\list-> sum ( map (\x-> boolToInt (k==x) ) list)) lists
-- numocc k = map (\list-> sum ( map (\x-> boolToInt (k==x) ) list))

eqTo k x= boolToInt (k==x)

-- numocc k = map (\list-> (sum) (map (eqTo k) list) ) 

-- numocc k = map (\list-> (sum) (flip map list (eqTo k)) ) 

-- numocc k = map (\list-> (sum) (   ((flip map list).(eqTo)) k   ) ) 

-- numocc k = map ((\list-> ((sum).(   ((flip map list).(eqTo))   )) k ) )

-- numocc k = map ((\list-> ((sum).(   ((flip map list).(eqTo))   )) k ) )

-- numocc k = map (  flip (\list k-> ((sum).(   ((flip map list).(eqTo))   )) k )  k )

-- numocc k = (map.(  flip (\list-> ((sum).(   ((flip map list).(eqTo))   )) )  )) k

numocc  = (map.(  flip (\list-> ((sum).(   ((flip map list).(eqTo))   )) )  )) 