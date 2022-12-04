
a 0 = 1
a n = (n-1) * (b (n-1)) - (3*(a (n-1)))

b 0 = 1
b n = 3*(b(n-1))+(((n-1)^2)*(a (n-1))) - (n-1)^2

anList n = [a k | k <- [1..n]]

seqIndex m =  (length (takeWhile (\x -> x) [ (sum (anList k)) < m | k <- [1..]])) + 1
