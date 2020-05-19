module Res where


import Lib.Peano
import Lib.Generator


-------------------------------------------------------------------------------------

leo0extII x0 x1 = leo0extII0 x0 x1 ++ leo0extII1 x0 x1
leo0extII0 s5@(O ) s6 = return $ ()
leo0extII0 _ _ = []
leo0extII1 s5@(S s8) s6@(S s9) = do
  () <- leo0extII s8 s9
  return $ ()
leo0extII1 _ _ = []

leo0extIO x0 = leo0extIO0 x0 ++ leo0extIO1 x0
leo0extIO0 s0@(O ) = do
  s1 <- (gen )
  return $ (s1)
leo0extIO0 _ = []
leo0extIO1 s0@(S s5) = do
  (s6) <- leo0extIO s5
  let s1 = (S s6)
  return $ (s1)
leo0extIO1 _ = []

leo1extII x0 x1 = leo1extII0 x0 x1 ++ leo1extII1 x0 x1
leo1extII0 s0@(S s4) s1@(O ) = return $ ()
leo1extII0 _ _ = []
leo1extII1 s0@(S s5) s1@(S s6) = do
  () <- leo1extII s5 s6
  return $ ()
leo1extII1 _ _ = []

minmaxoIIII x0 x1 x2 x3 = minmaxoIIII0 x0 x1 x2 x3 ++ minmaxoIIII1 x0 x1 x2 x3
minmaxoIIII0 s0 s1 s2@p3 s3@p1 | s0 == p3, s1 == p1 = do
  () <- leo0extII s0 s1
  return $ ()
minmaxoIIII0 _ _ _ _ = []
minmaxoIIII1 s0 s1 s2@p3 s3@p1 | s0 == p1, s1 == p3 = do
  () <- leo1extII s0 s1
  return $ ()
minmaxoIIII1 _ _ _ _ = []

minmaxoIOIO x0 x1 = minmaxoIOIO0 x0 x1 ++ minmaxoIOIO1 x0 x1
minmaxoIOIO0 s0 s2@p1 | s0 == p1 = do
  (s1) <- leo0extIO s0
  let s3 = s1
  return $ (s1, s3)
minmaxoIOIO0 _ _ = []
minmaxoIOIO1 s0 s2@s1 = do
  let s3 = s0
  (s1) <- leo1extIO s0
  return $ (s1, s3)
minmaxoIOIO1 _ _ = []

