module AFSyntax where


import           Data.List (intercalate)


data Pat = Var String
         | Ctor String [Pat]
         | Tuple [String]
  deriving (Eq, Ord)


data Expr = Term Pat
          | Call String [Pat]
  deriving (Eq, Ord)


data Assign = Assign Pat Expr
  deriving (Eq, Ord)

newtype Guard = Guard [Pat]
  deriving (Eq, Ord)

data Line = Line [Pat] [Guard] [Assign] Expr
  deriving (Eq, Ord)


data F = F String [Line]
  deriving (Eq, Ord)

-----------------------------------------------------------------------------------------------------

instance Show Pat where
  show (Var var)        = var
  show (Ctor name args) = ctor2str name (show <$> args)
    where
      ctor2str :: String -> [String] -> String
      ctor2str "Nil"   _      = "[]"
      ctor2str "Cons"  [x, y] = '(' : x ++ " : " ++ y ++ ")"
--      ctor2str "O"     _      = "0"
--      ctor2str "S"     [x]    = '(' : x ++ " + 1)"
      ctor2str "false" []     = show False
      ctor2str "true"  []     = show True
      ctor2str ctor    args   = '(' : ctor ++ " " ++ unwords args ++ ")"
  show (Tuple elems)    = '(' : intercalate ", " elems ++ ")"


instance Show Expr where
  show (Term pat)           = show pat
  show (Call funcName args) = funcName ++ " " ++ unwords (show <$> args)


instance Show Assign where
  show (Assign name expr@(Term _))   = "let " ++ show name ++ " = " ++ show expr
  show (Assign name expr@(Call _ _)) = show name ++ " <- " ++ show expr


instance Show Guard where
  show (Guard [])       = ""
  show (Guard (x : xs)) =
    let rvalue = show x ++ " == "
     in intercalate ", " . fmap ((rvalue ++) . show) $ xs 


instance Show Line where
  show (Line pats guards assigns expr) =
    (unwords . fmap show $ pats) ++
    printIfGuard guards ++ " = " ++
    printIfAssigns assigns ++
    printExpr expr
    where
      printIfGuard :: [Guard] -> String
      printIfGuard []     = ""
      printIfGuard guards = " | " ++ (intercalate ", " . fmap show $ guards)

      printIfAssigns :: [Assign] -> String
      printIfAssigns []      = ""
      printIfAssigns assigns = "do\n  " ++ (intercalate "\n  " . fmap show $ assigns) ++ "\n  "

      printExpr :: Expr -> String
      printExpr expr@(Term _) = "return $ " ++ show expr
      printExpr expr          = show expr


instance Show F where
  show (F funcName [])    = funcName ++ " = undefined\n"
  show (F funcName lines) = mainFunc ++ subFuncs
    where
      argsNum = getArgsNum lines
      subFuncsName = getNamesByNum funcName (length lines)
      
      mainFunc =
        let argsStr = unwords (getNamesByNum "x" argsNum)
         in funcName ++ " " ++ argsStr ++ " = "
            ++ (intercalate " ++ " . fmap (++ " " ++ argsStr) $ subFuncsName) ++ "\n"

      subFuncs = unlines (subFuncGen `concatMap` zip subFuncsName (show <$> lines))

      subFuncGen :: (String, String) -> [String]
      subFuncGen (name, body) =
        let skipBody = unwords (replicate argsNum "_") ++ " = []"
         in ((name ++ " ") ++) <$> (body : if (argsNum > 0) then [skipBody] else [])

      getNamesByNum :: String -> Int -> [String]
      getNamesByNum name num = ((name ++) . show) <$> [0..num - 1]
      
      getArgsNum :: [Line] -> Int
      getArgsNum ((Line pats _ _ _) : _) = length pats
      getArgsNum []                      = error "no lines"
      
