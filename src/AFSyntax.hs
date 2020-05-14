module AFSyntax where


import           Data.List   (intercalate)
import           Text.Printf (printf)


data Atom = Var String
          | Ctor String [Atom]
          | Tuple [String]
  deriving (Eq, Ord)


data Expr = Term Atom
          | Call String [Atom]
  deriving (Eq, Ord)


data Assign = Assign Atom Expr
  deriving (Eq, Ord)

newtype Guard = Guard [Atom]
  deriving (Eq, Ord)

data Pat = Pat (Maybe String) Atom
  deriving (Eq, Ord)

data Line = Line [Pat] [Guard] [Assign] [Guard] Expr
  deriving (Eq, Ord)


data F = F String [Line]
  deriving (Eq, Ord)


newtype HsProgram = HsProgram [F]

-----------------------------------------------------------------------------------------------------

instance Show Atom where
  show (Var var)        = var
  show (Ctor name args) = ctor2str name (show <$> args)
    where
      ctor2str :: String -> [String] -> String
      ctor2str "Nil"   _      = "[]"
      ctor2str "Cons"  [x, y] = printf "(%s : %s)" x y
--      ctor2str "O"     _      = "0"
--      ctor2str "S"     [x]    = '(' : x ++ " + 1)"
      ctor2str "false" []     = show False
      ctor2str "true"  []     = show True
      ctor2str ctor    args   = printf "(%s %s)" ctor (unwords args)
  show (Tuple elems)    = printf "(%s)" (intercalate ", " elems)


instance Show Expr where
  show (Term atom)          = show atom
  show (Call funcName args) = printf "%s %s" funcName (unwords . fmap show $ args)


instance Show Assign where
  show (Assign name expr@(Term (Ctor ctorName _)))
    | ctorName == "gen"            = printf "%s <- %s" (show name) (show expr)
  show (Assign name expr@(Term _)) = printf "let %s = %s" (show name) (show expr)
  show (Assign name expr)          = printf "%s <- %s" (show name) (show expr)


instance Show Guard where
  show (Guard [])       = ""
  show (Guard (x : xs)) =
    let rvalue = printf "%s == " (show x)
     in intercalate ", " . fmap ((rvalue ++) . show) $ xs 


instance Show Pat where
  show (Pat Nothing atom)     = show atom
  show (Pat (Just name) atom) = printf "%s@%s" name (show atom)


instance Show Line where
  show (Line pats patGuards assigns assignGuards expr) =
    (unwords . fmap show $ pats) ++
    printIfPatGuard patGuards ++ 
    printIfAssigns assigns ++
    printIfAssignGuard assignGuards expr
    where
      printIfPatGuard :: [Guard] -> String
      printIfPatGuard []     = " = "
      printIfPatGuard guards = printf " | %s = " (intercalate ", " . fmap show $ guards)

      printIfAssigns :: [Assign] -> String
      printIfAssigns []      = ""
      printIfAssigns assigns = printf "do\n  %s\n  " (intercalate "\n  " . fmap show $ assigns)

      printIfAssignGuard :: [Guard] -> Expr -> String
      printIfAssignGuard []     expr = printExpr expr
      printIfAssignGuard guards expr =
        printf "if (%s) then %s else []" (intercalate " && ". fmap show $ guards) (printExpr expr)

      printExpr :: Expr -> String
      printExpr expr@(Term _) = printf "return $ %s" (show expr)
      printExpr expr          = show expr


instance Show F where
  show (F funcName [])    = printf "%s = undefined\n" funcName
  show (F funcName lines) = mainFunc ++ subFuncs
    where
      argsNum = getArgsNum lines
      subFuncsName = getNamesByNum funcName (length lines)
      
      mainFunc =
        let argsStr = unwords (getNamesByNum "x" argsNum)
         in printf "%s %s = %s\n"
                   funcName argsStr (intercalate " ++ " . fmap (++ " " ++ argsStr) $ subFuncsName)

      subFuncs = unlines (subFuncGen `concatMap` zip subFuncsName (show <$> lines))

      subFuncGen :: (String, String) -> [String]
      subFuncGen (name, body) =
        let skipBody = printf "%s = []" (unwords $ replicate argsNum "_")
         in ((name ++ " ") ++) <$> (body : if (argsNum > 0) then [skipBody] else [])

      getNamesByNum :: String -> Int -> [String]
      getNamesByNum name num = ((name ++) . show) <$> [0..num - 1]
      
      getArgsNum :: [Line] -> Int
      getArgsNum ((Line pats _ _ _ _) : _) = length pats
      getArgsNum []                        = error "no lines"


instance Show HsProgram where
  show (HsProgram fList) =
    "-------------------------------------------------------------------------------------\n\n" ++
    (unlines . fmap show $ fList)

