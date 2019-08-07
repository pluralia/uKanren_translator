module AFSyntax where


data Pat = Var String
         | Ctor String [Pat]
  deriving (Eq, Ord, Show)

data Expr = Term Pat
          | Call String [Pat]
  deriving (Eq, Ord, Show)

data Assign = Assign String Expr
  deriving (Eq, Ord, Show)

data Line = Line [Pat] [Assign] Expr
  deriving (Eq, Ord, Show)

data F = F String [Line]
  deriving (Eq, Ord, Show)

