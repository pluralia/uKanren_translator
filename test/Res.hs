module Res where


import Lib.Peano
import Lib.Generator


-------------------------------------------------------------------------------------
{-
leoIII x0 x1 x2 = leoIII0 x0 x1 x2 ++ leoIII1 x0 x1 x2 ++ leoIII2 x0 x1 x2
leoIII0 s4@(O ) s5 s2@True = return $ ()
leoIII0 _ _ _ = []
leoIII1 s4@(S s6) s5@(O ) s2@False = return $ ()
leoIII1 _ _ _ = []
leoIII2 s4@(S s7) s5@(S s8) s2 = do
  (s7, s8) <- leoOOI s2
  return $ ()
leoIII2 _ _ _ = []
-}
leoIIO x0 x1 = leoIIO0 x0 x1 ++ leoIIO1 x0 x1 ++ leoIIO2 x0 x1
leoIIO0 s0@(O ) s1 = do
  let s2 = True
  return $ (s2)
leoIIO0 _ _ = []
leoIIO1 s0@(S s3) s1@(O ) = do
  let s2 = False
  return $ (s2)
leoIIO1 _ _ = []
leoIIO2 s0@(S s4) s1@(S s5) = do
  (s2) <- leoIIO s4 s5
  return $ (s2)
leoIIO2 _ _ = []
{-
-------------------------------------------------------------------------------------

leoIII x0 x1 x2 = leoIII0 x0 x1 x2 ++ leoIII1 x0 x1 x2 ++ leoIII2 x0 x1 x2
leoIII0 s4@(O ) s5 s2@True = return $ ()
leoIII0 _ _ _ = []
leoIII1 s4@(S s6) s5@(O ) s2@False = return $ ()
leoIII1 _ _ _ = []
leoIII2 s4@(S s7) s5@(S s8) s2 = do
  (s7, s8) <- leoOOI s2
  return $ ()
leoIII2 _ _ _ = []

leoOOI x0 = leoOOI0 x0 ++ leoOOI1 x0 ++ leoOOI2 x0
leoOOI0 s2@True = do
  let s0 = (O )
  s1 <- (gen )
  return $ (s0, s1)
leoOOI0 _ = []
leoOOI1 s2@False = do
  let s1 = (O )
  s3 <- (gen )
  let s0 = (S s3)
  return $ (s0, s1)
leoOOI1 _ = []
leoOOI2 s2 = do
  (s4, s5) <- leoOOI s2
  let s1 = (S s5)
  let s0 = (S s4)
  return $ (s0, s1)
leoOOI2 _ = []

-------------------------------------------------------------------------------------

leoIII x0 x1 x2 = leoIII0 x0 x1 x2 ++ leoIII1 x0 x1 x2 ++ leoIII2 x0 x1 x2
leoIII0 s4@(O ) s5 s2@True = return $ ()
leoIII0 _ _ _ = []
leoIII1 s4@(S s6) s5@(O ) s2@False = return $ ()
leoIII1 _ _ _ = []
leoIII2 s4@(S s7) s5@(S s8) s2 = do
  (s7, s8) <- leoOOI s2
  return $ ()
leoIII2 _ _ _ = []

leoIOI x0 x1 = leoIOI0 x0 x1 ++ leoIOI1 x0 x1 ++ leoIOI2 x0 x1
leoIOI0 s0@(O ) s2@True = do
  s1 <- (gen )
  return $ (s1)
leoIOI0 _ _ = []
leoIOI1 s0@(S s3) s2@False = do
  let s1 = (O )
  return $ (s1)
leoIOI1 _ _ = []
leoIOI2 s0@(S s4) s2 = do
  (s5) <- leoIOI s4 s2
  let s1 = (S s5)
  return $ (s1)
leoIOI2 _ _ = []

-------------------------------------------------------------------------------------

leoIII x0 x1 x2 = leoIII0 x0 x1 x2 ++ leoIII1 x0 x1 x2 ++ leoIII2 x0 x1 x2
leoIII0 s4@(O ) s5 s2@True = return $ ()
leoIII0 _ _ _ = []
leoIII1 s4@(S s6) s5@(O ) s2@False = return $ ()
leoIII1 _ _ _ = []
leoIII2 s4@(S s7) s5@(S s8) s2 = do
  (s7, s8) <- leoOOI s2
  return $ ()
leoIII2 _ _ _ = []

leoOII x0 x1 = leoOII0 x0 x1 ++ leoOII1 x0 x1 ++ leoOII2 x0 x1
leoOII0 s1 s2@True = do
  let s0 = (O )
  return $ (s0)
leoOII0 _ _ = []
leoOII1 s1@(O ) s2@False = do
  s3 <- (gen )
  let s0 = (S s3)
  return $ (s0)
leoOII1 _ _ = []
leoOII2 s1@(S s5) s2 = do
  (s4) <- leoOII s5 s2
  let s0 = (S s4)
  return $ (s0)
leoOII2 _ _ = []

-------------------------------------------------------------------------------------

leoIII x0 x1 x2 = leoIII0 x0 x1 x2 ++ leoIII1 x0 x1 x2 ++ leoIII2 x0 x1 x2
leoIII0 s4@(O ) s5 s2@True = return $ ()
leoIII0 _ _ _ = []
leoIII1 s4@(S s6) s5@(O ) s2@False = return $ ()
leoIII1 _ _ _ = []
leoIII2 s4@(S s7) s5@(S s8) s2 = do
  (s7, s8) <- leoOOI s2
  return $ ()
leoIII2 _ _ _ = []

leoIOO x0 = leoIOO0 x0 ++ leoIOO1 x0 ++ leoIOO2 x0
leoIOO0 s0@(O ) = do
  let s2 = True
  s1 <- (gen )
  return $ (s1, s2)
leoIOO0 _ = []
leoIOO1 s0@(S s3) = do
  let s1 = (O )
  let s2 = False
  return $ (s1, s2)
leoIOO1 _ = []
leoIOO2 s0@(S s4) = do
  (s5, s2) <- leoIOO s4
  let s1 = (S s5)
  return $ (s1, s2)
leoIOO2 _ = []

-------------------------------------------------------------------------------------

leoIII x0 x1 x2 = leoIII0 x0 x1 x2 ++ leoIII1 x0 x1 x2 ++ leoIII2 x0 x1 x2
leoIII0 s4@(O ) s5 s2@True = return $ ()
leoIII0 _ _ _ = []
leoIII1 s4@(S s6) s5@(O ) s2@False = return $ ()
leoIII1 _ _ _ = []
leoIII2 s4@(S s7) s5@(S s8) s2 = do
  (s7, s8) <- leoOOI s2
  return $ ()
leoIII2 _ _ _ = []

leoOIO x0 = leoOIO0 x0 ++ leoOIO1 x0 ++ leoOIO2 x0
leoOIO0 s1 = do
  let s2 = True
  let s0 = (O )
  return $ (s0, s2)
leoOIO0 _ = []
leoOIO1 s1@(O ) = do
  let s2 = False
  s3 <- (gen )
  let s0 = (S s3)
  return $ (s0, s2)
leoOIO1 _ = []
leoOIO2 s1@(S s5) = do
  (s4, s2) <- leoOIO s5
  let s0 = (S s4)
  return $ (s0, s2)
leoOIO2 _ = []

-------------------------------------------------------------------------------------

leoIII x0 x1 x2 = leoIII0 x0 x1 x2 ++ leoIII1 x0 x1 x2 ++ leoIII2 x0 x1 x2
leoIII0 s0@(O ) s1 s2@True = return $ ()
leoIII0 _ _ _ = []
leoIII1 s0@(S s3) s1@(O ) s2@False = return $ ()
leoIII1 _ _ _ = []
leoIII2 s0@(S s4) s1@(S s5) s2 = do
  (s4, s5) <- leoOOI s2
  return $ ()
leoIII2 _ _ _ = []

-------------------------------------------------------------------------------------

leoIII x0 x1 x2 = leoIII0 x0 x1 x2 ++ leoIII1 x0 x1 x2 ++ leoIII2 x0 x1 x2
leoIII0 s1@(O ) s2@True p0@True | s2 == p0 = return $ ()
leoIII0 _ _ _ = []
leoIII1 s1@(S s3) s2@(O ) p0@(O ) | s2 == p0 = if (s2 == False && s2 == False) then return $ () else []
leoIII1 _ _ _ = []
leoIII2 s1@(S s4) s2@(S s5) p0@(S p1) | s2 == p0, s5 == p1 = do
  (s4, s5) <- leoOOI s2
  return $ ()
leoIII2 _ _ _ = []

leoIIO x0 x1 = leoIIO0 x0 x1 ++ leoIIO1 x0 x1 ++ leoIIO2 x0 x1
leoIIO0 s4@(O ) s5 = do
  let s2 = True
  return $ (s2)
leoIIO0 _ _ = []
leoIIO1 s4@(S s0) s5@(O ) = do
  let s2 = False
  return $ (s2)
leoIIO1 _ _ = []
leoIIO2 s4@(S s1) s5@(S s2) = do
  (s1, s2, c0) <- leoOOO 
  if (s2 == c0) then return $ (s2) else []
leoIIO2 _ _ = []
-}
leoOOO  = leoOOO0  ++ leoOOO1  ++ leoOOO2 
leoOOO0  = do
  let s2 = True
  let s0 = (O )
  s1 <- (gen )
  return $ (s0, s1, s2)
leoOOO1  = do
  let s2 = False
  let s1 = (O )
  s3 <- (gen )
  let s0 = (S s3)
  return $ (s0, s1, s2)
leoOOO2  = do
  s5 <- (gen )
  s4 <- (gen )
  let s1 = (S s5)
  let s0 = (S s4)
  (s2) <- leoIIO s4 s5
  return $ (s0, s1, s2)

