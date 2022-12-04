

digitsSum 0 = 0 
digitsSum n = (digitsSum (div n 10) ) + ((n `mod` 10) )


sevensHelper n 7 = n
sevensHelper n i =  sevensHelper (digitsSum n) (i + 1)


sevens n = take n [ x | x<-[1..], (sevensHelper x 0) == 7]