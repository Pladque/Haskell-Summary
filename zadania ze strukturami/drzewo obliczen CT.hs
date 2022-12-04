data CT a = Empty | Leaf a | Join (CT a) Op (CT a)

data Op = Add | Mul | Neg

-- a)
areBothEmpty Empty Empty = False
areBothEmpty l r = True


wf :: CT a -> Bool
wf Empty = False
wf (Join Empty Add r) = False
wf (Join l Add Empty) = False
wf (Join l Mul Empty) = False
wf (Join Empty Mul r) = False
wf (Join l Neg r) = areBothEmpty l r
wf (Join l op r) = (wf l) && (wf r)
wf ct = True

t = (Join (Join (Leaf 3) Add (Leaf 2)) Mul (Join (Leaf 2) Neg Empty))


-- b)
calc :: (Num a) => Op->a->a-> a
calc Add x y= x+y
calc Mul x y= x*y
calc Neg x y= -x-y

eval :: (Num a) => CT a -> a
eval t = evalHelper t (wf t)



-- jesli drzewo nie bd poprawne, pojawi sioe blad bo nie dopasuje do wzorca
evalHelper :: (Num a) => (CT a) ->Bool -> a
evalHelper Empty True = 0
evalHelper (Leaf k) True = k
evalHelper (Join l op r) True = calc op (evalHelper (l) True) (evalHelper (r) True)