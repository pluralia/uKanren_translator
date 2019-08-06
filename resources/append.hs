-- R in abc => translateSig: a -> b -> c 
append :: [a] -> [a] -> [[a]]
append []      y = [y]
append (h : t) y = fmap (h :) (append t y)
append _       _ = []
