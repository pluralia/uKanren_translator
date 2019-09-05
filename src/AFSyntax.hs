module AFSyntax where


data Pat = Var String
         | Ctor String [Pat]
  deriving (Eq, Ord)


data Expr = Term Pat
          | Call String [Pat]
  deriving (Eq, Ord)


data Assign = Assign String Expr
  deriving (Eq, Ord)

newtype Guard = Guard [String]
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
      ctor2str "Nil"  _      = "[]"
      ctor2str "Cons" [x, y] = '(' : x ++ " : " ++ y ++ ")"
      ctor2str "O"    _      = "0"
      ctor2str "S"    [x]    = '(' : x ++ " + 1)"
      ctor2str ctor   args   = '(' : ctor ++ " " ++ unwords args ++ ")"


instance Show Expr where
  show (Term pat)           = show pat
  show (Call funcName args) = funcName ++ ' ' : unwords (show <$> args)


instance Show Assign where
  show (Assign name expr) = name ++ " = " ++ show expr


instance Show Guard where
  show (Guard [])          = ""
  show (Guard (name : xs)) =
    let rvalue = ", " ++ name ++ " == "
     in drop 2 . unwords . fmap (rvalue ++) $ xs 


instance Show Line where
  show (Line pats guards assigns expr) =
    (unwords $ show <$> pats) ++ printIfGuard guards ++ " = " ++ show expr ++ printIfAssigns assigns 
    where
      printIfGuard :: [Guard] -> String
      printIfGuard []     = ""
      printIfGuard guards = " | " ++ (drop 2 . unwords . fmap ((", " ++) . show) $ guards)

      printIfAssigns :: [Assign] -> String
      printIfAssigns []      = ""
      printIfAssigns assigns =  "\n  where\n" ++ (unlines $ (("    " ++) . show) <$> assigns)


instance Show F where
  show (F funcName lines) = unlines $ ((funcName ++ " ") ++) . show <$> lines

