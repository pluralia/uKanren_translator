module Revacco where


import Lib.Peano
import Lib.Generator


-------------------------------------------------------------------------------------

revaccoIOO x0 = revaccoIOO0 x0 ++ revaccoIOO1 x0
revaccoIOO0 s0@[] = do
  s1 <- (gen :: [[Int]])
  let s2 = s1
  return $ (s1, s2)
revaccoIOO0 _ = []
revaccoIOO1 s0@(s3 : s4) = do
  ((s3' : s1), s2) <- revaccoIOO s4
  if (s3 == s3') then return $ (s1, s2) else []
revaccoIOO1 _ = []

-------------------------------------------------------------------------------------

revaccoOII x0 x1 = revaccoOII0 x0 x1 ++ revaccoOII1 x0 x1
revaccoOII0 s1 s2@p1 | s1 == p1 = do
  let s0 = []
  return $ (s0)
revaccoOII0 _ _ = []
revaccoOII1 s1 s2 = do
  (s4, (s3 : s1')) <- revaccoOOI s2
  let s0 = (s3 : s4)
  if (s1 == s1') then return $ (s0) else []
revaccoOII1 _ _ = []

-------------------------------------------------------------------------------------

revaccoOIO x0 = revaccoOIO0 x0 ++ revaccoOIO1 x0
revaccoOIO0 s1 = do
  let s2 = s1
  let s0 = []
  return $ (s0, s2)
revaccoOIO0 _ = []
revaccoOIO1 s1 = do
  s3 <- (gen :: [Int])
  s4 <- (gen )
  let s5 = (s3 : s1)
  let s0 = (s3 : s4)
  (s2) <- revaccoIIO s5 s4
  return $ (s0, s2)
revaccoOIO1 _ = []

-------------------------------------------------------------------------------------

revaccoIOI x0 x1 = revaccoIOI0 x0 x1 ++ revaccoIOI1 x0 x1
revaccoIOI0 s0@[] s2@s1 = return $ (s1)
revaccoIOI0 _ _ = []
revaccoIOI1 s0@(s3 : s4) s2 = do
  ((s3 : s1)) <- revaccoIOI s4 s2
  return $ (s1)
revaccoIOI1 _ _ = []

-------------------------------------------------------------------------------------

revaccoOOI x0 = revaccoOOI0 x0 ++ revaccoOOI1 x0
revaccoOOI0 s2@s1 = do
  let s0 = []
  return $ (s0, s1)
revaccoOOI0 _ = []
revaccoOOI1 s2 = do
  (s4, (s3 : s1)) <- revaccoOOI s2
  let s0 = (s3 : s4)
  return $ (s0, s1)
revaccoOOI1 _ = []

-------------------------------------------------------------------------------------

revaccoIIO x0 x1 = revaccoIIO0 x0 x1 ++ revaccoIIO1 x0 x1
revaccoIIO0 s0@[] s1 = do
  let s2 = s1
  return $ (s2)
revaccoIIO0 _ _ = []
revaccoIIO1 s0@(s3 : s4) s1 = do
  let s5 = (s3 : s1)
  (s2) <- revaccoIIO s4 s5
  return $ (s2)
revaccoIIO1 _ _ = []

-------------------------------------------------------------------------------------

revaccoIII x0 x1 x2 = revaccoIII0 x0 x1 x2 ++ revaccoIII1 x0 x1 x2
revaccoIII0 s0@[] s1 s2@p1 | s1 == p1 = return $ ()
revaccoIII0 _ _ _ = []
revaccoIII1 s0@(s3 : s4) s1 s2 = do
  let s5 = (s3 : s1)
  (c0) <- revaccoIOI s4 s2
  if (s5 == c0) then return $ () else []
revaccoIII1 _ _ _ = []

-------------------------------------------------------------------------------------

revaccoOOO  = revaccoOOO0  ++ revaccoOOO1 
revaccoOOO0  = do
  s1 <- (gen :: [[Int]])
  let s0 = []
  let s2 = s1
  return $ (s0, s1, s2)
revaccoOOO1  = do
  s1 <- (gen )
  s3 <- (gen )
  s4 <- (gen )
  let s5 = (s3 : s1)
  let s0 = (s3 : s4)
  (c1, s2) <- revaccoIOO s4
  if (s5 == c1) then return $ (s0, s1, s2) else []
