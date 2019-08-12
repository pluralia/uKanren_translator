appendo [] y = y
appendo (h : t) y = (h : ty)
  where
    strange = t
ty = appendo t y

