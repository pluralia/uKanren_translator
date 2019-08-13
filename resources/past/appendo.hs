appendo [] y = y
appendo (h : t) y = (h : ty)
  where
    strange = t
    STRANGE = h
    ty = appendo t y

