appendo [] y = return $ y
appendo (h : t) y = do
  ty <- appendo t y
  return $ (h : ty)
appendo _ _ = fail "Illegal arguments"

appendoX xy xy' | xy == xy' = return $ []
appendoX y (h : ty) = do
  t <- appendoX y ty
  return $ (h : t)
appendoX _ _ = fail "Illegal arguments"

appendoXX xy xy' | xy == xy' = return $ []
appendoXX y (h : ty) = do
  t <- appendoXX y ty
  return $ (h : t)
appendoXX _ _ = fail "Illegal arguments"

copy [] = return $ []
copy (h : t) = do
  t1 <- copy t
  return $ (h : t1)
copy _ = fail "Illegal arguments"

revacco [] acc = return $ acc
revacco (h : t) acc = revacco t (h : acc)
revacco _ _ = fail "Illegal arguments"

lengtho [] = return $ 0
lengtho (h : t) = do
  z <- lengtho t
  return $ (z + 1)
lengtho _ = fail "Illegal arguments"

copy2 [] = return $ []
copy2 (h : []) = return $ (h : [])
copy2 (h1 : (h2 : t)) = do
  t1 <- copy2 t
  return $ (h1 : t1)
copy2 _ = fail "Illegal arguments"

