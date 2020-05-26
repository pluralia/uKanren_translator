module PermSort where


import Lib.Peano
import Lib.Generator

-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------

permSortIO x0 = permSortIO0 x0 ++ permSortIO1 x0
permSortIO0 s0@[] = do
  let s1 = []
  return $ (s1)
permSortIO0 _ = []
permSortIO1 s0 = do
  (s2, s3) <- smallestoIOO s0
  (s4) <- permSortIO s3
  let s1 = (s2 : s4)
  return $ (s1)
permSortIO1 _ = []

leo0extII x0 x1 = leo0extII0 x0 x1 ++ leo0extII1 x0 x1
leo0extII0 s5@(O ) s9 = return $ ()
leo0extII0 _ _ = []
leo0extII1 s5@(S s11) s9@(S s12) = do
  () <- leo0extII s11 s12
  return $ ()
leo0extII1 _ _ = []

leo1extII x0 x1 = leo1extII0 x0 x1 ++ leo1extII1 x0 x1
leo1extII0 s5@(S s10) s9@(O ) = return $ ()
leo1extII0 _ _ = []
leo1extII1 s5@(S s11) s9@(S s12) = do
  () <- leo1extII s11 s12
  return $ ()
leo1extII1 _ _ = []

minmaxoIIOO x0 x1 = minmaxoIIOO0 x0 x1 ++ minmaxoIIOO1 x0 x1
minmaxoIIOO0 s5 s9 = do
  () <- leo0extII s5 s9
  let s2 = s5
  let s7 = s9
  return $ (s2, s7)
minmaxoIIOO0 _ _ = []
minmaxoIIOO1 s5 s9 = do
  () <- leo1extII s5 s9
  let s2 = s9
  let s7 = s5
  return $ (s2, s7)
minmaxoIIOO1 _ _ = []

smallestoIOO x0 = smallestoIOO0 x0 ++ smallestoIOO1 x0
smallestoIOO0 s0@(s2 : []) = do
  let s3 = []
  return $ (s2, s3)
smallestoIOO0 _ = []
smallestoIOO1 s0@(s5 : s6) = do
  (s9, s8) <- smallestoIOO s6
  (s2, s7) <- minmaxoIIOO s5 s9
  let s3 = (s7 : s8)
  return $ (s2, s3)
smallestoIOO1 _ = []

-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------

permSortOI x0 = permSortOI0 x0 ++ permSortOI1 x0
permSortOI0 s1@[] = do
  let s0 = []
  return $ (s0)
permSortOI0 _ = []
permSortOI1 s1@(s2 : s4) = do
  (s3) <- permSortOI s4
  (s0) <- smallestoOII s2 s3
  return $ (s0)
permSortOI1 _ = []

leo0extOI x0 = leo0extOI0 x0 ++ leo0extOI1 x0
leo0extOI0 s12 = do
  let s8 = (O )
  return $ (s8)
leo0extOI0 _ = []
leo0extOI1 s12@(S s15) = do
  (s14) <- leo0extOI s15
  let s8 = (S s14)
  return $ (s8)
leo0extOI1 _ = []

minmaxoOIOI x0 x1 = minmaxoOIOI0 x0 x1 ++ minmaxoOIOI1 x0 x1
minmaxoOIOI0 s12 s10@p1 | s12 == p1 = do
  (s8) <- leo0extOI s12
  let s7 = s8
  return $ (s8, s7)
minmaxoOIOI0 _ _ = []
minmaxoOIOI1 s12 s10@s8 = do
  let s7 = s12
  () <- leo1extII s8 s12
  return $ (s8, s7)
minmaxoOIOI1 _ _ = []

minmaxoOIII x0 x1 x2 = minmaxoOIII0 x0 x1 x2 ++ minmaxoOIII1 x0 x1 x2
minmaxoOIII0 s7 s1@s3 s5@p1 | s7 == p1 = do
  () <- leo0extII s3 s7
  return $ (s3)
minmaxoOIII0 _ _ _ = []
minmaxoOIII1 s7 s1@p3 s5@s3 | s7 == p3 = do
  () <- leo1extII s3 s7
  return $ (s3)
minmaxoOIII1 _ _ _ = []

smallestoOII x0 x1 = smallestoOII0 x0 x1 ++ smallestoOII1 x0 x1
smallestoOII0 s1 s2@[] = do
  let s0 = (s1 : [])
  return $ (s0)
smallestoOII0 _ _ = []
smallestoOII1 s1 s2@(s5 : s6) = do
  (s4, s7) <- smallestoOOI s6
  (s3) <- minmaxoOIII s7 s1 s5
  let s0 = (s3 : s4)
  return $ (s0)
smallestoOII1 _ _ = []

smallestoOOI x0 = smallestoOOI0 x0 ++ smallestoOOI1 x0
smallestoOOI0 s6@[] = do
  s7 <- (gen )
  let s4 = (s7 : [])
  return $ (s4, s7)
smallestoOOI0 _ = []
smallestoOOI1 s6@(s10 : s11) = do
  (s9, s12) <- smallestoOOI s11
  (s8, s7) <- minmaxoOIOI s12 s10
  let s4 = (s8 : s9)
  return $ (s4, s7)
smallestoOOI1 _ = []

-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
{-
permSortOI x0 = permSortOI0 x0 ++ permSortOI1 x0
permSortOI0 s1@[] = do
  let s0 = []
  return $ (s0)
permSortOI0 _ = []
permSortOI1 s1@(s2 : s4) = do
  (s0, s3) <- smallestoOIO s2
  (c0) <- permSortOI s4
  if (s3 == c0) then return $ (s0) else []
permSortOI1 _ = []

leo0extIO x0 = leo0extIO0 x0 ++ leo0extIO1 x0
leo0extIO0 s5@(O ) = do
  s9 <- (gen )
  return $ (s9)
leo0extIO0 _ = []
leo0extIO1 s5@(S s11) = do
  (s12) <- leo0extIO s11
  let s9 = (S s12)
  return $ (s9)
leo0extIO1 _ = []

leo1extOI x0 = leo1extOI0 x0 ++ leo1extOI1 x0
leo1extOI0 s9@(O ) = do
  s10 <- (gen )
  let s5 = (S s10)
  return $ (s5)
leo1extOI0 _ = []
leo1extOI1 s9@(S s12) = do
  (s11) <- leo1extOI s12
  let s5 = (S s11)
  return $ (s5)
leo1extOI1 _ = []

minmaxoOOIO x0 = minmaxoOOIO0 x0 ++ minmaxoOOIO1 x0
minmaxoOOIO0 s2@s5 = do
  (s9) <- leo0extIO s5
  let s7 = s9
  return $ (s5, s9, s7)
minmaxoOOIO0 _ = []
minmaxoOOIO1 s2@s9 = do
  (s5) <- leo1extOI s9
  let s7 = s5
  return $ (s5, s9, s7)
minmaxoOOIO1 _ = []

smallestoOIO x0 = smallestoOIO0 x0 ++ smallestoOIO1 x0
smallestoOIO0 s2 = do
  let s0 = (s2 : [])
  let s3 = []
  return $ (s0, s3)
smallestoOIO0 _ = []
smallestoOIO1 s2 = do
  (s5, s9, s7) <- minmaxoOOIO s2
  (s6, s8) <- smallestoOIO s9
  let s0 = (s5 : s6)
  let s3 = (s7 : s8)
  return $ (s0, s3)
smallestoOIO1 _ = []
-}
