appendo [] y = y
appendo (h : t) y = (h : ty)
  where
    ty = appendo t y

