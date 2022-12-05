data Wr a = Wierzba Int [[a]]


dg (Wierzba k galezie) nowaGalaz = (Wierzba (k+1) (nowaGalaz:galezie))

ug (Wierzba k galezie) = (Wierzba (k-1)  (drop 1 galezie))


de (Wierzba k galezie) newElem = (Wierzba k ((newElem : (head galezie)) : (tail galezie)) )


ue (Wierzba k galezie) = (Wierzba k ((drop 1 (head galezie)) : (tail galezie)) )

lg (Wierzba k galezie) = k

wr2l (Wierzba k galezie) = concat galezie
