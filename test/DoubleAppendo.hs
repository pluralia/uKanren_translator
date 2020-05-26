module DoubleAppendo (
  doubleAppendoIO
, doubleAppendoOI
, doubleAppendoII
, doubleAppendoOO
) where


import Lib.Peano
import Lib.Generator
import Appendo

-------------------------------------------------------------------------------------

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

appendo0extOI x0 = appendo0extOI0 x0 ++ appendo0extOI1 x0
appendo0extOI0 s1@s0 = do
  let s0' = []
  if (s0 == s0') then return $ (s0) else []
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

appendo0extII x0 x1 = appendo0extII0 x0 x1 ++ appendo0extII1 x0 x1
appendo0extII0 s0@[] s1@p1 | s0 == p1 = return $ ()
appendo0extII0 _ _ = []
appendo0extII1 s0@(s2 : s3) s1@(p2 : s4) | s2 == p2 = do
  () <- appendoIII s3 s0 s4
  return $ ()
appendo0extII1 _ _ = []

doubleAppendoII x0 x1 = doubleAppendoII0 x0 x1
doubleAppendoII0 s0 s1 = do
  () <- appendo0extII s0 s1
  return $ ()
doubleAppendoII0 _ _ = []

-------------------------------------------------------------------------------------

appendo0extOO  = appendo0extOO0  ++ appendo0extOO1 
appendo0extOO0 = do
  let s0 = []
  let s1 = s0
  return $ (s0, s1)
appendo0extOO1 = do
  s4 <- (gen :: [[Int]])
  s2 <- (gen :: [Int])
  s3 <- (gen )
  let s1 = (s2 : s4)
  let s0 = (s2 : s3)
  (s4) <- appendoIIO s3 s1
  return $ (s0, s1)

doubleAppendoOO  = doubleAppendoOO0 
doubleAppendoOO0  = do
  (s0, s1) <- appendo0extOO
  return $ (s0, s1)

