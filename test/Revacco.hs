module Revacco where


import Lib.Peano
import Lib.Generator

-------------------------------------------------------------------------------------
-- modifs in translator:
-- 1) repeated vars in pattern matching and assigns
-- 2) let (s1 : s2) = s3 should be removed: 
--    all occurences of s3 have to be replaced by (s1 : s2)
-------------------------------------------------------------------------------------

-- OK with modifs
-- s1 <- (gen :: [[Int]])
-- take 10 $ revaccoIOO [1, 2]
revaccoIOO x0 = revaccoIOO0 x0 ++ revaccoIOO1 x0
revaccoIOO0 s0@[] = do
  s2 <- (gen :: [[Int]])
  return $ (s2, s2)
revaccoIOO0 _ = []
revaccoIOO1 s0@(s3 : s4) = do
  ((s3' : s1), s2) <- revaccoIOO s4
  if (s3 == s3') then return $ (s1, s2) else []
revaccoIOO1 _ = []

-------------------------------------------------------------------------------------

-- FAIL
-- call revaccoIOO (and without generation (fixed))
-- s3 <- (gen :: [Int])
-- take 10 $ revaccoOIO [1, 2]
revaccoOIO x0 = revaccoOIO0 x0 ++ revaccoOIO1 x0
revaccoOIO0 s2 = do
  let s0 = []
  return $ (s0, s2)
revaccoOIO0 _ = []
revaccoOIO1 s1 = do
  s3 <- (gen :: [Int])
  s4 <- (gen )
  let s5 = (s3 : s1)
  let s0 = (s3 : s4)
  (c1, s2) <- revaccoIOO s4
  if (s5 == c1) then return $ (s0, s2) else []
revaccoOIO1 _ = []

-------------------------------------------------------------------------------------

-- OK: maybe modifs
-- list all results and stick
-- revaccoOOI [1..3]
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

-- OK: maybe modifs
-- revaccoIIO [1..3] [4]
revaccoIIO x0 x1 = revaccoIIO0 x0 x1 ++ revaccoIIO1 x0 x1
revaccoIIO0 s0@[] s2 = do
  return $ (s2)
revaccoIIO0 _ _ = []
revaccoIIO1 s0@(s3 : s4) s1 = do
  let s5 = (s3 : s1)
  (s2) <- revaccoIIO s4 s5
  return $ (s2)
revaccoIIO1 _ _ = []

-------------------------------------------------------------------------------------

-- OK: maybe modifs
-- revaccoIOI [1..3] [3, 2, 1, 4, 5]
revaccoIOI x0 x1 = revaccoIOI0 x0 x1 ++ revaccoIOI1 x0 x1
revaccoIOI0 s0@[] s2@s1 = return $ (s1)
revaccoIOI0 _ _ = []
revaccoIOI1 s0@(s3 : s4) s2 = do
  ((s3 : s1)) <- revaccoIOI s4 s2
  return $ (s1)
revaccoIOI1 _ _ = []

-------------------------------------------------------------------------------------

-- OK with modifs
-- list all results and stick
-- call revaccoOOI (and without generation (fixed))
-- revaccoOII [4, 5] [3, 2, 1, 4, 5]
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
