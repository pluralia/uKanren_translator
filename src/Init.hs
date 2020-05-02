module Init where

import qualified Data.Map.Strict as M
import           Data.Maybe           (fromMaybe)
import           Data.List            (nub, union)

import           Program.Prop
import           Syntax

import           Parser               (defsAsts)

-----------------------------------------------------------------------------------------------------

initDefsByNames :: [String] -> IO (M.Map Name Def)
initDefsByNames inFileNames = do
  listListDefs <- mapM
                    (\fileName -> do
                        input <- readFile fileName
                        return . defsAsts $ input
                    )
                    inFileNames
  return . M.fromList . fmap (\def@(Def name _ _) -> (name, def)) . concat $ listListDefs

-----------------------------------------------------------------------------------------------------

createProgram :: M.Map Name Def -> Name -> [Name] -> Program
createProgram nameToDef name args =
  let goal = makeFresh name args in Program (defsByGoal [] goal) goal
  where
    makeFresh :: Name -> [Name] -> G X
    makeFresh name args = fresh args $ Invoke name (V <$> args)

    defsByNames :: M.Map Name Def -> [Name] -> [Def]
    defsByNames nameToDef =
      fmap (\name -> fromMaybe (error $ "no func: " ++ name) $ M.lookup name nameToDef)

    defsByGoal :: [Def] -> G X -> [Def]
    defsByGoal knownDefs goal =
      let defs       = nub . defsByNames nameToDef . namesOfInvokes $ goal
          newDefs    = filter (`notElem` knownDefs) defs
       in defs `union` concatMap (\(Def _ _ goal) -> defsByGoal defs goal) newDefs
    
    namesOfInvokes :: G X -> [Name]
    namesOfInvokes (_ :=: _)       = []
    namesOfInvokes (g1 :/\: g2)    = namesOfInvokes g1 ++ namesOfInvokes g2
    namesOfInvokes (g1 :\/: g2)    = namesOfInvokes g1 ++ namesOfInvokes g2
    namesOfInvokes (Fresh _ g)     = namesOfInvokes g
    namesOfInvokes (Invoke name _) = [name]
    namesOfInvokes (Let _ _)       = error "LET"

-----------------------------------------------------------------------------------------------------

