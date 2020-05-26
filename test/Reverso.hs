module Reverso (
  reversoIO
, reversoOI
, reversoII 
, reversoOO 
) where


import Lib.Peano
import Lib.Generator

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

reversoIO x0 = reversoIO0 x0 ++ reversoIO1 x0
reversoIO0 s0@[] = do
  let s1 = []
  return $ (s1)
reversoIO0 _ = []
reversoIO1 s0@(s2 : s3) = do
  (s4) <- reversoIO s3
  (s1) <- appendo0extIIO s4 s2
  return $ (s1)
reversoIO1 _ = []

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

reversoOI x0 = reversoOI0 x0 ++ reversoOI1 x0
reversoOI0 s1@[] = do
  let s0 = []
  return $ (s0)
reversoOI0 _ = []
reversoOI1 s1 = do
  (s4, s2) <- appendo0extOOI s1
  (s3) <- reversoOI s4
  let s0 = (s2 : s3)
  return $ (s0)
reversoOI1 _ = []

-------------------------------------------------------------------------------------

appendo0extOII x0 x1 = appendo0extOII0 x0 x1 ++ appendo0extOII1 x0 x1
appendo0extOII0 s2 s3@(p1 : []) | s2 == p1 = do
  let s1 = []
  return $ (s1)
appendo0extOII0 _ _ = []
appendo0extOII1 s2 s3@(s4 : s6) = do
  (s5) <- appendo0extOII s2 s6
  let s1 = (s4 : s5)
  return $ (s1)
appendo0extOII1 _ _ = []

reversoII x0 x1 = reversoII0 x0 x1 ++ reversoII1 x0 x1
reversoII0 s0@[] s1@[] = return $ ()
reversoII0 _ _ = []
reversoII1 s0@(s2 : s3) s1 = do
  (s4) <- reversoIO s3
  (c0) <- appendo0extOII s2 s1
  if (s4 == c0) then return $ () else []
reversoII1 _ _ = []

-------------------------------------------------------------------------------------

reversoOO  = reversoOO0  ++ reversoOO1 
reversoOO0  = do
  let s1 = []
  let s0 = []
  return $ (s0, s1)
reversoOO1  = do
  s2 <- (gen :: [Int])
  s3 <- (gen )
  let s0 = (s2 : s3)
  (s4) <- reversoIO s3
  (s1) <- appendo0extIIO s4 s2
  return $ (s0, s1)

