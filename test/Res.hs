module Res where


import Lib.Peano
import Lib.Generator


-------------------------------------------------------------------------------------

leo0extII x0 x1 = leo0extII0 x0 x1 ++ leo0extII1 x0 x1
leo0extII0 s5@(O ) s6 = return $ ()
leo0extII0 _ _ = []
leo0extII1 s5@(S s8) s6@(S s9) = do
  leo0extII s8 s9
  return $ ()
leo0extII1 _ _ = []

leo1extII x0 x1 = leo1extII0 x0 x1 ++ leo1extII1 x0 x1
leo1extII0 s0@(S s4) s1@(O ) = return $ ()
leo1extII0 _ _ = []
leo1extII1 s0@(S s5) s1@(S s6) = do
  leo1extII s5 s6
  return $ ()
leo1extII1 _ _ = []

minmaxoIIII x0 x1 x2 x3 = minmaxoIIII0 x0 x1 x2 x3 ++ minmaxoIIII1 x0 x1 x2 x3
minmaxoIIII0 s0 s1 s2@p3 s3@p1 | s0 == p3, s1 == p1 = do
  leo0extII s0 s1
  return $ ()
minmaxoIIII0 _ _ _ _ = []
minmaxoIIII1 s0 s1 s2@p3 s3@p1 | s0 == p1, s1 == p3 = do
  leo1extII s0 s1
  return $ ()
minmaxoIIII1 _ _ _ _ = []

minmaxoIIOO x0 x1 = minmaxoIIOO0 x0 x1 ++ minmaxoIIOO1 x0 x1
minmaxoIIOO0 s0 s1 = do
  leo0extII s0 s1
  let s2 = s0
  let s3 = s1
  return $ (s2, s3)
minmaxoIIOO0 _ _ = []
minmaxoIIOO1 s0 s1 = do
  leo1extII s0 s1
  let s2 = s1
  let s3 = s0
  return $ (s2, s3)
minmaxoIIOO1 _ _ = []

