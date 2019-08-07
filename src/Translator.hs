module Translator (
      translator
    , strTranslator
    ) where

import           AFSyntax
import           Syntax

-----------------------------------------------------------------------------------------------------

translator :: Maybe Def -> Maybe F
translator = fmap go

strTranslator :: Maybe Def -> IO ()
strTranslator = maybe (putStrLn "Your program is not parsed") (print . go)

-----------------------------------------------------------------------------------------------------

chooseDirection :: [String] -> (String, [String])
chooseDirection vars = (last vars, init vars)

go :: Def -> F
go (name, vars, goal) = F name . fmap (toLine (chooseDirection vars)) . splitOnDisj $ goal

-----------------------------------------------------------------------------------------------------

splitOnDisj :: G X -> [G X]
splitOnDisj (g1 :\/: g2) = g1 : splitOnDisj g2
splitOnDisj g            = [g]

splitOnConj :: G X -> [G X]
splitOnConj (g1 :/\: g2) = g1 : splitOnConj g2
splitOnConj g            = [g]

-----------------------------------------------------------------------------------------------------

toLine :: (String, [String]) -> G X -> Line
toLine (res, args) goal = Line ((head . patWalk conjs) <$> args) [] (toExpr conjs res)
  where
    conjs = splitOnConj goal

patWalk :: [G X] -> String -> [Pat]
patWalk []                 arg = [Var arg]
patWalk ((t1 :=: t2) : ts) arg
  | t1 == V arg = term2pat t2 : patWalk ts arg
  | t2 == V arg = term2pat t1 : patWalk ts arg
  | otherwise   = patWalk ts arg
patWalk (_ : ts)           arg = patWalk ts arg

term2pat :: Term String -> Pat
term2pat (V var)        = Var var
term2pat (C name terms) = Ctor name (fmap term2pat terms)

toExpr :: [G X] -> String -> Expr
toExpr conjs res = go $ patWalk conjs res
  where
    go :: [Pat] -> Expr
    go ((Var res) : ps)    = go ps
    go (c@(Ctor _ _) : ps) = Term c
    -- случай, когда pat не зашёл!!! Надо рассматривать другие конструкторы
    go []                  = Term $ Var "UNDEFINED"
