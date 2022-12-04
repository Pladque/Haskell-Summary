pownum :: Integer -> [Integer]

-- n[0]^k+n[1]^k+...+n[-1]^k
digitsPowerSum 0 k = 0 :: Integer
digitsPowerSum n k = (digitsPowerSum (div n 10) k ) + ((n `mod` 10) ^ k )

pownum n = [digitsPowerSum x n| x<-[1..], (digitsPowerSum x n) == x]