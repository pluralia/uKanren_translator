module AppendoCtorUnif where


import Lib.Peano
import Lib.Generator


-------------------------------------------------------------------------------------

appendoCtorUnifIIO x0 x1 = appendoCtorUnifIIO0 x0 x1 ++ appendoCtorUnifIIO1 x0 x1
appendoCtorUnifIIO0 s0@[] s1 = do
  let s2 = s1
  return $ (s2)
appendoCtorUnifIIO0 _ _ = []
appendoCtorUnifIIO1 s0@(s3 : s4) s1 = do
  (s5) <- appendoCtorUnifIIO s4 s1
  let s2 = (s3 : s5)
  return $ (s2)
appendoCtorUnifIIO1 _ _ = []

-------------------------------------------------------------------------------------

appendoCtorUnifOOI x0 = appendoCtorUnifOOI0 x0 ++ appendoCtorUnifOOI1 x0
appendoCtorUnifOOI0 s2@s1 = do
  let s0 = []
  return $ (s0, s1)
appendoCtorUnifOOI0 _ = []
appendoCtorUnifOOI1 s2@(s3 : s5) = do
  (s4, s1) <- appendoCtorUnifOOI s5
  let s0 = (s3 : s4)
  return $ (s0, s1)
appendoCtorUnifOOI1 _ = []

-------------------------------------------------------------------------------------

appendoCtorUnifIOO x0 = appendoCtorUnifIOO0 x0 ++ appendoCtorUnifIOO1 x0
appendoCtorUnifIOO0 s0@[] = do
  s1 <- (gen )
  let s2 = s1
  return $ (s1, s2)
appendoCtorUnifIOO0 _ = []
appendoCtorUnifIOO1 s0@(s3 : s4) = do
  (s1, s5) <- appendoCtorUnifIOO s4
  let s2 = (s3 : s5)
  return $ (s1, s2)
appendoCtorUnifIOO1 _ = []

-------------------------------------------------------------------------------------

appendoCtorUnifOII x0 x1 = appendoCtorUnifOII0 x0 x1 ++ appendoCtorUnifOII1 x0 x1
appendoCtorUnifOII0 s1 s2@p1 | s1 == p1 = do
  let s0 = []
  return $ (s0)
appendoCtorUnifOII0 _ _ = []
appendoCtorUnifOII1 s1 s2@(s3 : s5) = do
  (s4) <- appendoCtorUnifOII s1 s5
  let s0 = (s3 : s4)
  return $ (s0)
appendoCtorUnifOII1 _ _ = []

-------------------------------------------------------------------------------------

appendoCtorUnifOIO x0 = appendoCtorUnifOIO0 x0 ++ appendoCtorUnifOIO1 x0
appendoCtorUnifOIO0 s1 = do
  let s2 = s1
  let s0 = []
  return $ (s0, s2)
appendoCtorUnifOIO0 _ = []
appendoCtorUnifOIO1 s1 = do
  s3 <- (gen )
  (s4, s5) <- appendoCtorUnifOIO s1
  let s2 = (s3 : s5)
  let s0 = (s3 : s4)
  return $ (s0, s2)
appendoCtorUnifOIO1 _ = []

-------------------------------------------------------------------------------------

appendoCtorUnifIOI x0 x1 = appendoCtorUnifIOI0 x0 x1 ++ appendoCtorUnifIOI1 x0 x1
appendoCtorUnifIOI0 s0@[] s2@s1 = return $ (s1)
appendoCtorUnifIOI0 _ _ = []
appendoCtorUnifIOI1 s0@(s3 : s4) s2@(p2 : s5) | s3 == p2 = do
  (s1) <- appendoCtorUnifIOI s4 s5
  return $ (s1)
appendoCtorUnifIOI1 _ _ = []

-------------------------------------------------------------------------------------

appendoCtorUnifIII x0 x1 x2 = appendoCtorUnifIII0 x0 x1 x2 ++ appendoCtorUnifIII1 x0 x1 x2
appendoCtorUnifIII0 s0@[] s1 s2@p1 | s1 == p1 = return $ ()
appendoCtorUnifIII0 _ _ _ = []
appendoCtorUnifIII1 s0@(s3 : s4) s1 s2@(p2 : s5) | s3 == p2 = do
  () <- appendoCtorUnifIII s4 s1 s5
  return $ ()
appendoCtorUnifIII1 _ _ _ = []

-------------------------------------------------------------------------------------

appendoCtorUnifOOO  = appendoCtorUnifOOO0  ++ appendoCtorUnifOOO1 
appendoCtorUnifOOO0  = do
  s1 <- (gen :: [[Int]])
  let s0 = []
  let s2 = s1
  return $ (s0, s1, s2)
appendoCtorUnifOOO1  = do
  s5 <- (gen )
  s3 <- (gen )
  s4 <- (gen )
  let s2 = (s3 : s5)
  let s0 = (s3 : s4)
  (s1) <- appendoCtorUnifIOI s4 s5
  return $ (s0, s1, s2)

