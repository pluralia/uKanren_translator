module Res where


import Lib.Peano
import Lib.Generator


-------------------------------------------------------------------------------------

leo0extII x0 x1 = leo0extII0 x0 x1 ++ leo0extII1 x0 x1
leo0extII0 s8@(O ) s12 = return $ ()
leo0extII0 _ _ = []
leo0extII1 s8@(S s14) s12@(S s15) = do
  () <- leo0extII s14 s15
  return $ ()
leo0extII1 _ _ = []

leo1extII x0 x1 = leo1extII0 x0 x1 ++ leo1extII1 x0 x1
leo1extII0 s8@(S s13) s12@(O ) = return $ ()
leo1extII0 _ _ = []
leo1extII1 s8@(S s14) s12@(S s15) = do
  () <- leo1extII s14 s15
  return $ ()
leo1extII1 _ _ = []

minmaxoIIII x0 x1 x2 x3 = minmaxoIIII0 x0 x1 x2 x3 ++ minmaxoIIII1 x0 x1 x2 x3
minmaxoIIII0 s18 s22 s17@p3 s20@p1 | s18 == p3, s22 == p1 = do
  () <- leo0extII s18 s22
  return $ ()
minmaxoIIII0 _ _ _ _ = []
minmaxoIIII1 s18 s22 s17@p3 s20@p1 | s18 == p1, s22 == p3 = do
  () <- leo1extII s18 s22
  return $ ()
minmaxoIIII1 _ _ _ _ = []

minmaxoIIOI x0 x1 x2 = minmaxoIIOI0 x0 x1 x2 ++ minmaxoIIOI1 x0 x1 x2
minmaxoIIOI0 s18 s22 s20@p1 | s22 == p1 = do
  () <- leo0extII s18 s22
  let s17 = s18
  return $ (s17)
minmaxoIIOI0 _ _ _ = []
minmaxoIIOI1 s18 s22 s20@p1 | s18 == p1 = do
  () <- leo1extII s18 s22
  let s17 = s22
  return $ (s17)
minmaxoIIOI1 _ _ _ = []

minmaxoIIIO x0 x1 x2 = minmaxoIIIO0 x0 x1 x2 ++ minmaxoIIIO1 x0 x1 x2
minmaxoIIIO0 s3 s7 s1@p1 | s3 == p1 = do
  () <- leo0extII s3 s7
  let s5 = s7
  return $ (s5)
minmaxoIIIO0 _ _ _ = []
minmaxoIIIO1 s3 s7 s1@p1 | s7 == p1 = do
  () <- leo1extII s3 s7
  let s5 = s3
  return $ (s5)
minmaxoIIIO1 _ _ _ = []

minmaxoIIOO x0 x1 = minmaxoIIOO0 x0 x1 ++ minmaxoIIOO1 x0 x1
minmaxoIIOO0 s8 s12 = do
  () <- leo0extII s8 s12
  let s7 = s8
  let s10 = s12
  return $ (s7, s10)
minmaxoIIOO0 _ _ = []
minmaxoIIOO1 s8 s12 = do
  () <- leo1extII s8 s12
  let s7 = s12
  let s10 = s8
  return $ (s7, s10)
minmaxoIIOO1 _ _ = []

smallestoIII x0 x1 x2 = smallestoIII0 x0 x1 x2 ++ smallestoIII1 x0 x1 x2
smallestoIII0 s9@(s12 : []) p1 s11@[] | s12 == p1 = return $ ()
smallestoIII0 _ _ _ = []
smallestoIII1 s9@(s13 : s14) s12 s11@(s15 : s16) = do
  (s17) <- smallestoIOI s14 s16
  (c0) <- minmaxoIOII s13 s12 s15
  if (s17 == c0) then return $ () else []
smallestoIII1 _ _ _ = []

smallestoIIO x0 x1 = smallestoIIO0 x0 x1 ++ smallestoIIO1 x0 x1
smallestoIIO0 s0@(s1 : []) p0 | s1 == p0 = do
  let s2 = []
  return $ (s2)
smallestoIIO0 _ _ = []
smallestoIIO1 s0@(s3 : s4) s1 = do
  (s7, s6) <- smallestoIOO s4
  (s5) <- minmaxoIIIO s3 s7 s1
  let s2 = (s5 : s6)
  return $ (s2)
smallestoIIO1 _ _ = []

smallestoIOI x0 x1 = smallestoIOI0 x0 x1 ++ smallestoIOI1 x0 x1
smallestoIOI0 s14@(s17 : []) s16@[] = return $ (s17)
smallestoIOI0 _ _ = []
smallestoIOI1 s14@(s18 : s19) s16@(s20 : s21) = do
  (s22) <- smallestoIOI s19 s21
  (s17) <- minmaxoIIOI s18 s22 s20
  return $ (s17)
smallestoIOI1 _ _ = []

smallestoIOO x0 = smallestoIOO0 x0 ++ smallestoIOO1 x0
smallestoIOO0 s4@(s7 : []) = do
  let s6 = []
  return $ (s7, s6)
smallestoIOO0 _ = []
smallestoIOO1 s4@(s8 : s9) = do
  (s12, s11) <- smallestoIOO s9
  (s7, s10) <- minmaxoIIOO s8 s12
  let s6 = (s10 : s11)
  return $ (s7, s6)
smallestoIOO1 _ = []

