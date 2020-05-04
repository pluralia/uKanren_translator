module Res where


import Lib.Peano
import Lib.Generator


-------------------------------------------------------------------------------------

appendoIIO x0 x1 = appendoIIO0 x0 x1 ++ appendoIIO1 x0 x1
appendoIIO0 s0@[] s1 = do
  let s2 = s1
  return $ (s2)
appendoIIO0 _ _ = []
appendoIIO1 s0@(s3 : s4) s1 = do
  (s5) <- appendoIIO s4 s1
  let s2 = (s3 : s5)
  return $ (s2)
appendoIIO1 _ _ = []

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

appendoOOI x0 = appendoOOI0 x0 ++ appendoOOI1 x0
appendoOOI0 s2@s1 = do
  let s0 = []
  return $ (s0, s1)
appendoOOI0 _ = []
appendoOOI1 s2@(s3 : s5) = do
  (s4, s1) <- appendoOOI s5
  let s0 = (s3 : s4)
  return $ (s0, s1)
appendoOOI1 _ = []

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

appendoIOO x0 = appendoIOO0 x0 ++ appendoIOO1 x0
appendoIOO0 s0@[] = do
  s1 <- (gen )
  let s2 = s1
  return $ (s1, s2)
appendoIOO0 _ = []
appendoIOO1 s0@(s3 : s4) = do
  (s1, s5) <- appendoIOO s4
  let s2 = (s3 : s5)
  return $ (s1, s2)
appendoIOO1 _ = []

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

appendoOIO x0 = appendoOIO0 x0 ++ appendoOIO1 x0
appendoOIO0 s1 = do
  let s2 = s1
  let s0 = []
  return $ (s0, s2)
appendoOIO0 _ = []
appendoOIO1 s1 = do
  s3 <- (gen )
  (s4, s5) <- appendoOIO s1
  let s2 = (s3 : s5)
  let s0 = (s3 : s4)
  return $ (s0, s2)
appendoOIO1 _ = []

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

appendoOII x0 x1 = appendoOII0 x0 x1 ++ appendoOII1 x0 x1
appendoOII0 s1 s2@p1 | s1 == p1 = do
  let s0 = []
  return $ (s0)
appendoOII0 _ _ = []
appendoOII1 s1 s2@(s3 : s5) = do
  (s4) <- appendoOII s1 s5
  let s0 = (s3 : s4)
  return $ (s0)
appendoOII1 _ _ = []

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

appendoIOI x0 x1 = appendoIOI0 x0 x1 ++ appendoIOI1 x0 x1
appendoIOI0 s0@[] s2@s1 = return $ (s1)
appendoIOI0 _ _ = []
appendoIOI1 s0@(s3 : s4) s2@(p2 : s5) | s3 == p2 = do
  (s1) <- appendoIOI s4 s5
  return $ (s1)
appendoIOI1 _ _ = []

-------------------------------------------------------------------------------------
