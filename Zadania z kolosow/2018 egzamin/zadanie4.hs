
divisors n = [x | x <- [1..(n-1)], n `rem` x == 0]

perfect::Integer -> Bool

perfect n = (sum (divisors n )) == n