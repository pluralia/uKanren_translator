import Peano

maxo1 = undefined

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

lengtho [] = return $ (O )
lengtho (h : t) = do
  z <- lengtho t
  return $ (S z)
lengtho _ = fail "Illegal arguments"

copy2 [] = return $ []
copy2 (h : []) = return $ (h : [])
copy2 (h1 : (h2 : t)) = do
  t1 <- copy2 t
  return $ (h1 : t1)
copy2 _ = fail "Illegal arguments"

maxLengtho x m = do
  m <- maxo x
  lengtho x
maxLengtho _ _ = fail "Illegal arguments"

copycopy l l1 = do
  l1 <- copy l
  copy2 l
copycopy _ _ = fail "Illegal arguments"

maxo x = maxo1 x (O )
maxo _ = fail "Illegal arguments"

reverso [] = return $ []
reverso (h : t) = do
  rt <- reverso t
  appendo rt (h : [])
reverso _ = fail "Illegal arguments"

addo (O ) y = return $ y
addo (S x1) y = do
  z1 <- addo x1 y
  return $ (S z1)
addo _ _ = fail "Illegal arguments"

mulo (O ) y = return $ (O )
mulo (S x1) y = do
  z1 <- mulo x1 y
  addo y z1
mulo _ _ = fail "Illegal arguments"

nando False False = return $ True
nando False True = return $ True
nando True False = return $ True
nando True True = return $ False
nando _ _ = fail "Illegal arguments"

noto a = nando a a
noto _ = fail "Illegal arguments"

oro a b = do
  bb <- nando b b 
  aa <- nando a a
  nando aa bb
oro _ _ = fail "Illegal arguments"

ando a b = do
  ab <- nando a b
  nando ab ab
ando _ _ = fail "Illegal arguments"

doubleAppendo x y z = do
  t <- appendo x y
  appendo t z
doubleAppendo _ _ _ = fail "Illegal arguments"

doubleo x = appendo x x
doubleo _ = fail "Illegal arguments"

emptyAppendo x = appendo [] x
emptyAppendo _ = fail "Illegal arguments"

appendo123 x = appendo ((S (O )) : ((S (S (O ))) : ((S (S (S (O )))) : []))) x
appendo123 _ = fail "Illegal arguments"

appendoXyz x y z t = appendo (x : (y : (z : []))) t
appendoXyz _ _ _ _ = fail "Illegal arguments"

test (x : []) = do
  mulx <- mulo x (S (S (O )))
  return $ (x : (mulx : []))
test l = do
  x2 <- test ((S (S (O ))) : []) 
  x1 <- test ((S (O )) : [])
  appendo x1 x2
test _ = fail "Illegal arguments"

