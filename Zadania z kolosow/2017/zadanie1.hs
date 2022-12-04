-- rd :: Integer -> [Integer]

-- sprawdza czy 'i' jedzie dzielnikiem k i potem sprawdza dla i-1
znajdzDzielniki k 1 = [1]
znajdzDzielniki k i | k `mod` i == 0= i: (znajdzDzielniki k (i-1)) | otherwise=znajdzDzielniki k (i-1)

generatePrimes = [ k | k<-[1..], (length (znajdzDzielniki k k)) == 2]

rd 1 = generatePrimes
rd n = [ k | k<-[1..n^2], (sum (znajdzDzielniki k k)) == (n + k )]
