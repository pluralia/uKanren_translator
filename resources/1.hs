import Peano

maxo1 = undefined

makeX  = makeX0  ++ makeX1 
makeX0  = return $ (O )
makeX1  = return $ (S (O ))

makeY x0 = makeY0 x0 ++ makeY1 x0 ++ makeY2 x0
makeY0 x = return $ x
makeY0 _ = []
makeY1 x = return $ (S (S (O )))
makeY1 _ = []
makeY2 x = return $ (S (S (S (O ))))
makeY2 _ = []

makeA  = makeA0  ++ makeA1 
makeA0  = return $ (S (S (S (S (O )))))
makeA1  = return $ (S (S (S (S (S (O ))))))

makeB  = makeB0  ++ makeB1 
makeB0  = return $ (S (S (S (S (S (S (O )))))))
makeB1  = return $ (S (S (S (S (S (S (S (O ))))))))

go x0 = go0 x0 ++ go1 x0
go0 ((S (O )) : (sec : rest)) = do
  x <- makeX  
  y <- makeY x
  return $ (x : (y : []))
go0 _ = []
go1 (fir : ((S (S (O ))) : rest)) = do
  a <- makeA  
  b <- makeB 
  return $ (a : (b : []))
go1 _ = []

