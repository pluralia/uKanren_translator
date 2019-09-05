appendo [] y = y
appendo (h : t) y = (h : ty)
  where
    ty = appendo t y


appendoX xy xy' | xy == xy' = []
appendoX y (h : ty) = (h : t)
  where
    t = appendoX y ty


copy [] = []
copy (h : t) = (h : t1)
  where
    t1 = copy t


revacco [] acc = acc
revacco (h : t) acc = revacco t (h : acc)

lengtho [] = 0
lengtho (h : t) = (z + 1)
  where
    z = lengtho t


copy2 [] = []
copy2 (h : []) = (h : [])
copy2 (h1 : (h2 : t)) = (h1 : t1)
  where
    t1 = copy2 t


