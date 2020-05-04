module Res where


import Lib.Peano
import Lib.Generator


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

appendoIOI x0 x1 = appendoIOI0 x0 x1 ++ appendoIOI1 x0 x1
appendoIOI0 s0@[] s2@s1 = return $ (s1)
appendoIOI0 _ _ = []
appendoIOI1 s0@(s3 : s4) s2@(p2 : s5) | s3 == p2 = do
  (s1) <- appendoIOI s4 s5
  return $ (s1)
appendoIOI1 _ _ = []

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

appendoPatIIO x0 x1 = appendoPatIIO0 x0 x1 ++ appendoPatIIO1 x0 x1
appendoPatIIO0 s0@[] s1 = do
  let s2 = s1
  return $ (s2)
appendoPatIIO0 _ _ = []
appendoPatIIO1 s0@((s6 : s7) : s8) s1 = do
  let s9 = s0
  let (s3 : s4) = s0
  (s5) <- appendoPatIIO s4 s1
  let s2 = (s3 : s5)
  return $ (s2)
appendoPatIIO1 _ _ = []

-------------------------------------------------------------------------------------

appendoPatIOO x0 = appendoPatIOO0 x0 ++ appendoPatIOO1 x0
appendoPatIOO0 s0@[] = do
  s1 <- (gen )
  let s2 = s1
  return $ (s1, s2)
appendoPatIOO0 _ = []
appendoPatIOO1 s0@((s6 : s7) : s8) = do
  let (s3 : s4) = s0
  let s9 = s0
  (s1, s5) <- appendoPatIOO s4
  let s2 = (s3 : s5)
  return $ (s1, s2)
appendoPatIOO1 _ = []

-------------------------------------------------------------------------------------

appendoPatIOI x0 x1 = appendoPatIOI0 x0 x1 ++ appendoPatIOI1 x0 x1
appendoPatIOI0 s0@[] s2@s1 = return $ (s1)
appendoPatIOI0 _ _ = []
appendoPatIOI1 s0@((s6 : s7) : s8) s2@(s3 : s5) = do
  let s9 = s0
  let (s3 : s4) = s0
  (s1) <- appendoPatIOI s4 s5
  return $ (s1)
appendoPatIOI1 _ _ = []

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

appendoCtorsUnifIIO x0 x1 = appendoCtorsUnifIIO0 x0 x1 ++ appendoCtorsUnifIIO1 x0 x1
appendoCtorsUnifIIO0 s0@[] s1 = do
  let s2 = s1
  return $ (s2)
appendoCtorsUnifIIO0 _ _ = []
appendoCtorsUnifIIO1 s0@(s3 : s4) s1 = do
  (s5) <- appendoCtorsUnifIIO s4 s1
  let s2 = (s3 : s5)
  return $ (s2)
appendoCtorsUnifIIO1 _ _ = []

-------------------------------------------------------------------------------------

appendoAssignIIO x0 x1 = appendoAssignIIO0 x0 x1 ++ appendoAssignIIO1 x0 x1
appendoAssignIIO0 s0@[] s1 = do
  let s2 = s1
  return $ (s2)
appendoAssignIIO0 _ _ = []
appendoAssignIIO1 s0@(s3 : s4) s1 = do
  (s5) <- appendoAssignIIO s4 s1
  let s2 = (s3 : s5)
  let (s6 : (c4 : s7)) = s2
  let (s8 : (c1 : s9)) = s2
  if (s6 == c4 && s8 == c1) then return $ (s2) else []
appendoAssignIIO1 _ _ = []

-------------------------------------------------------------------------------------

appendoAssignOOI x0 = appendoAssignOOI0 x0 ++ appendoAssignOOI1 x0
appendoAssignOOI0 s2@s1 = do
  let s0 = []
  return $ (s0, s1)
appendoAssignOOI0 _ = []
appendoAssignOOI1 s2@(s8 : (p2 : s9)) | s8 == p2 = do
  let (s3 : s5) = s2
  let (s6 : (c4 : s7)) = s2
  (s4, s1) <- appendoAssignOOI s5
  let s0 = (s3 : s4)
  if (s6 == c4) then return $ (s0, s1) else []
appendoAssignOOI1 _ = []

-------------------------------------------------------------------------------------

appendoAssignOII x0 x1 = appendoAssignOII0 x0 x1 ++ appendoAssignOII1 x0 x1
appendoAssignOII0 s1 s2@p1 | s1 == p1 = do
  let s0 = []
  return $ (s0)
appendoAssignOII0 _ _ = []
appendoAssignOII1 s1 s2@(s8 : (p2 : s9)) | s8 == p2 = do
  let (s3 : s5) = s2
  let (s6 : (c3 : s7)) = s2
  (s4) <- appendoAssignOII s1 s5
  let s0 = (s3 : s4)
  if (s6 == c3) then return $ (s0) else []
appendoAssignOII1 _ _ = []

-------------------------------------------------------------------------------------

appendoAssignIOI x0 x1 = appendoAssignIOI0 x0 x1 ++ appendoAssignIOI1 x0 x1
appendoAssignIOI0 s0@[] s2@s1 = return $ (s1)
appendoAssignIOI0 _ _ = []
appendoAssignIOI1 s0@(s3 : s4) s2@(s8 : (p2 : s9)) | s8 == p2 = do
  let (s3 : s5) = s2
  let (s6 : (c2 : s7)) = s2
  (s1) <- appendoAssignIOI s4 s5
  if (s6 == c2) then return $ (s1) else []
appendoAssignIOI1 _ _ = []

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

appendoIIO x0 x1 = appendoIIO0 x0 x1 ++ appendoIIO1 x0 x1
appendoIIO0 s3@[] s0 = do
  let s4 = s0
  return $ (s4)
appendoIIO0 _ _ = []
appendoIIO1 s3@(s5 : s6) s0 = do
  (s7) <- appendoIIO s6 s0
  let s4 = (s5 : s7)
  return $ (s4)
appendoIIO1 _ _ = []

appendo0extIO x0 = appendo0extIO0 x0 ++ appendo0extIO1 x0
appendo0extIO0 s0@[] = do
  let s1 = s0
  return $ (s1)
appendo0extIO0 _ = []
appendo0extIO1 s0@(s2 : s3) = do
  (s4) <- appendoIIO s3 s0
  let s1 = (s2 : s4)
  return $ (s1)
appendo0extIO1 _ = []

doubleAppendoIO x0 = doubleAppendoIO0 x0
doubleAppendoIO0 s0 = do
  (s1) <- appendo0extIO s0
  return $ (s1)
doubleAppendoIO0 _ = []

-------------------------------------------------------------------------------------

appendoOOI x0 = appendoOOI0 x0 ++ appendoOOI1 x0
appendoOOI0 s4@s0 = do
  let s3 = []
  return $ (s3, s0)
appendoOOI0 _ = []
appendoOOI1 s4@(s5 : s7) = do
  (s6, s0) <- appendoOOI s7
  let s3 = (s5 : s6)
  return $ (s3, s0)
appendoOOI1 _ = []

appendo0extOI x0 = appendo0extOI0 x0 ++ appendo0extOI1 x0
appendo0extOI0 s1@s0 = do
  let s0 = []
  return $ (s0)
appendo0extOI0 _ = []
appendo0extOI1 s1@(s2 : s4) = do
  (s3, s0) <- appendoOOI s4
  if (s0 == (s2 : s3)) then return $ (s0) else []
appendo0extOI1 _ = []

doubleAppendoOI x0 = doubleAppendoOI0 x0
doubleAppendoOI0 s1 = do
  (s0) <- appendo0extOI s1
  return $ (s0)
doubleAppendoOI0 _ = []

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

lengthoIO x0 = lengthoIO0 x0 ++ lengthoIO1 x0
lengthoIO0 s0@[] = do
  let s1 = (O )
  return $ (s1)
lengthoIO0 _ = []
lengthoIO1 s0@(s2 : s3) = do
  (s4) <- lengthoIO s3
  let s1 = (S s4)
  return $ (s1)
lengthoIO1 _ = []

-------------------------------------------------------------------------------------

lengthoOI x0 = lengthoOI0 x0 ++ lengthoOI1 x0
lengthoOI0 s1@(O ) = do
  let s0 = []
  return $ (s0)
lengthoOI0 _ = []
lengthoOI1 s1@(S s4) = do
  s2 <- (gen )
  (s3) <- lengthoOI s4
  let s0 = (s2 : s3)
  return $ (s0)
lengthoOI1 _ = []

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

appendo0extIIO x0 x1 = appendo0extIIO0 x0 x1 ++ appendo0extIIO1 x0 x1
appendo0extIIO0 s4@[] s2 = do
  let s1 = (s2 : [])
  return $ (s1)
appendo0extIIO0 _ _ = []
appendo0extIIO1 s4@(s5 : s6) s2 = do
  (s7) <- appendo0extIIO s6 s2
  let s1 = (s5 : s7)
  return $ (s1)
appendo0extIIO1 _ _ = []

reversoRevIO x0 = reversoRevIO0 x0 ++ reversoRevIO1 x0
reversoRevIO0 s0@[] = do
  let s1 = []
  return $ (s1)
reversoRevIO0 _ = []
reversoRevIO1 s0@(s2 : s3) = do
  (s4) <- reversoRevIO s3
  (s1) <- appendo0extIIO s4 s2
  return $ (s1)
reversoRevIO1 _ = []

-------------------------------------------------------------------------------------

appendo0extOOI x0 = appendo0extOOI0 x0 ++ appendo0extOOI1 x0
appendo0extOOI0 s1@(s2 : []) = do
  let s4 = []
  return $ (s4, s2)
appendo0extOOI0 _ = []
appendo0extOOI1 s1@(s5 : s7) = do
  (s6, s2) <- appendo0extOOI s7
  let s4 = (s5 : s6)
  return $ (s4, s2)
appendo0extOOI1 _ = []

reversoRevOI x0 = reversoRevOI0 x0 ++ reversoRevOI1 x0
reversoRevOI0 s1@[] = do
  let s0 = []
  return $ (s0)
reversoRevOI0 _ = []
reversoRevOI1 s1 = do
  (s4, s2) <- appendo0extOOI s1
  (s3) <- reversoRevOI s4
  let s0 = (s2 : s3)
  return $ (s0)
reversoRevOI1 _ = []

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

revaccoIOO x0 = revaccoIOO0 x0 ++ revaccoIOO1 x0
revaccoIOO0 s0@[] = do
  s1 <- (gen :: [[Int]])
  let s2 = s1
  return $ (s1, s2)
revaccoIOO0 _ = []
revaccoIOO1 s0@(s3 : s4) = do
  (s5, s2) <- revaccoIOO s4
  let (s3 : s1) = s5
  return $ (s1, s2)
revaccoIOO1 _ = []

-------------------------------------------------------------------------------------

revaccoOIO x0 = revaccoOIO0 x0 ++ revaccoOIO1 x0
revaccoOIO0 s1 = do
  let s2 = s1
  let s0 = []
  return $ (s0, s2)
revaccoOIO0 _ = []
revaccoOIO1 s1 = do
  s3 <- (gen  :: [Int])
  s4 <- (gen )
  let s5 = (s3 : s1)
  let s0 = (s3 : s4)
  (c1, s2) <- revaccoIOO s4
  if (s5 == c1) then return $ (s0, s2) else []
revaccoOIO1 _ = []

-------------------------------------------------------------------------------------

revaccoOOI x0 = revaccoOOI0 x0 ++ revaccoOOI1 x0
revaccoOOI0 s2@s1 = do
  let s0 = []
  return $ (s0, s1)
revaccoOOI0 _ = []
revaccoOOI1 s2 = do
  (s4, s5) <- revaccoOOI s2
  let (s3 : s1) = s5
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

revaccoIOI x0 x1 = revaccoIOI0 x0 x1 ++ revaccoIOI1 x0 x1
revaccoIOI0 s0@[] s2@s1 = return $ (s1)
revaccoIOI0 _ _ = []
revaccoIOI1 s0@(s3 : s4) s2 = do
  (s5) <- revaccoIOI s4 s2
  let (s3 : s1) = s5
  return $ (s1)
revaccoIOI1 _ _ = []

-------------------------------------------------------------------------------------

revaccoOII x0 x1 = revaccoOII0 x0 x1 ++ revaccoOII1 x0 x1
revaccoOII0 s1 s2@p1 | s1 == p1 = do
  let s0 = []
  return $ (s0)
revaccoOII0 _ _ = []
revaccoOII1 s1 s2 = do
  (s4, s5) <- revaccoOOI s2
  let (s3 : s1) = s5
  let s0 = (s3 : s4)
  return $ (s0)
revaccoOII1 _ _ = []

-------------------------------------------------------------------------------------
