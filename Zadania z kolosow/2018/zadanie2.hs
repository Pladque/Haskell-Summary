-- zle to rozwiazanie chyba

data Expr a = Value a | Add (Expr a) (Expr a)| Mul (Expr a) (Expr a)| Sub (Expr a) (Expr a)| P


eq op (Value x) (Value y) = x==y
eq op P P = True
eq op (Add v1 v2) P = True
eq op P (Add v1 v2) = True
eq op (Mul v1 v2) P = True
eq op P (Mul v1 v2) = True
eq op (Sub v1 v2) P = True
eq op P (Sub v1 v2) = True
eq op (Add v11 v12) (Add v21 v22) = (eq v11 v12) && (eq v21 v22)
eq op (Mul v11 v12) (Mul v21 v22) = (eq v11 v12) && (eq v21 v22)
eq op (Sub v11 v12) (Sub v21 v22) = (eq v11 v12) && (eq v21 v22)



expr1 = Add (Sub (Add (Value 0) (Value 0)) (Add (Value 0) (Value 0))) (Add (Value 1) (Value 3))
expr2 = Sub (Add (Value 0) (Value 0)) (Add (Value 0) (Value 0))
