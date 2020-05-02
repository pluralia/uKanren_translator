{-# LANGUAGE TupleSections #-}
module Annotator.Internal.Stack (
    addToStack
  , disjStackPred
  , maybeStack
  ) where


import qualified Data.Map.Strict  as M
import           Data.Maybe            (fromMaybe, isJust, catMaybes, isNothing)
import qualified Data.Set         as S

import           Syntax

import           Annotator.Internal.Lib
import           Annotator.Internal.Types

import           Debug.Trace           (trace)

----------------------------------------------------------------------------------------------------

maybeStack :: Stack -> Maybe Stack
maybeStack stack =
  let 
      filteredArgsOrderStack = M.map (S.filter argsOrderPred) $ stack
      filteredStack          = M.filter (not . S.null) filteredArgsOrderStack
   in if filteredArgsOrderStack == filteredStack then Just filteredStack else Nothing

----------------------------------------------------------------------------------------------------

argsOrderPred :: ArgsOrder -> Bool
argsOrderPred (ArgsOrder anns goal _) =
  all isJust anns &&
  (all (isJust . snd) . concatMap (concatMap getVars) $ goal)


disjStackPred :: Name -> ([G (S, Ann)], Stack) -> Bool
disjStackPred name (list, stD) = isNoUndef && isAllInvDef
  where
    isNoUndef :: Bool
    isNoUndef = all isJust . concatMap (fmap snd . getVars) $ list

    isAllInvDef :: Bool
    isAllInvDef  = all isDefInv . filter isNotRecInvoke $ list
      where
        isNotRecInvoke :: G a -> Bool
        isNotRecInvoke (Invoke name' _) = not $ name == name'
        isNotRecInvoke _                = False

        isDefInv :: G a -> Bool
        isDefInv (Invoke name _) = maybe False (any argsOrderPred) $ M.lookup name stD
        isDefInv _               = False

----------------------------------------------------------------------------------------------------

addToStack :: Stack -> Name -> [Term (S, Ann)] -> [[G (S, Ann)]] -> Stack
addToStack stack name terms goal = {- trace ("addToStack: " ++ name ++ " | " ++ show terms ++ " | " ++ show (argsOrder terms goal)) $ -}
  let
      argsOrderSet    = fromMaybe S.empty $ M.lookup name stack
      updArgsOrderSet = S.insert (argsOrder terms goal) argsOrderSet
   in M.insert name updArgsOrderSet stack

----------------------------------------------------------------------------------------------------

