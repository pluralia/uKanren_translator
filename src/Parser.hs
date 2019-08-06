{-# LANGUAGE DerivingStrategies #-}

module Parser (
      getStrAstWithDefGoal
    , getProgAst
    , getDefAsts
    ) where

import           Control.Monad (void)
import           Control.Monad.Combinators.Expr
import           Data.Void (Void(..))
import           Data.Either (either)
import           Data.Foldable (foldl')
import           Syntax
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-----------------------------------------------------------------------------------------------------

getStrAstWithDefGoal :: String -> String
getStrAstWithDefGoal = either errorBundlePretty (show . ($ V "a" === V "b")) . runParser parseProg ""

getProgAst :: String -> Maybe (G X -> G X)
getProgAst = either (const Nothing) Just . runParser parseProg ""

getDefAsts :: String -> Maybe [G X -> G X]
getDefAsts = either (const Nothing) Just . runParser parseDefs ""

-----------------------------------------------------------------------------------------------------

type Parser = Parsec Void String


-- spaces & comments
sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

sugar :: [String]
sugar = ["trueo", "falseo", "zero", "succ", "conde"]

ident :: Parser String
ident = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many (char '_' <|> alphaNumChar)
    check x = if x `elem` sugar
                then fail $ show x ++ " cannot be an identifier"
                else return x

-- brackets
roundBr, angleBr, boxBr, curvyBr :: Parser a -> Parser a
roundBr = between (symbol "(") (symbol ")")
angleBr = between (symbol "<") (symbol ">")
boxBr   = between (symbol "[") (symbol "]")
curvyBr = between (symbol "{") (symbol "}")


-- term
parseTerm :: Parser (Term X)
parseTerm = parseListTerm


parseListTerm :: Parser (Term X)
parseListTerm = try conso
  <|> try (roundBr conso)
  <|> niloOrNum

niloOrNum :: Parser (Term X)
niloOrNum = try parseNumTerm <|> do
  symbol "[]"
  return $ C "Nil" []

conso :: Parser (Term X)
conso = do
  x  <- niloOrNum
  xs <- some $ symbol "%" *> niloOrNum
  return $ foldl' (\term acc -> C "Cons" [term, acc]) x xs


parseNumTerm :: Parser (Term X)
parseNumTerm = try succo
  <|> try (roundBr succo)
  <|> zeroOrBool

zeroOrBool :: Parser (Term X)
zeroOrBool = try parseBoolTerm <|> do
  symbol "zero"
  return $ C "O" []

succo :: Parser (Term X)
succo = do
  symbol "succ"
  x <- try zeroOrBool <|> roundBr succo
  return $ C "S" [x]


parseBoolTerm :: Parser (Term X)
parseBoolTerm = try falseo
  <|> try trueo
  <|> parseDesugarTerm

trueo :: Parser (Term X)
trueo = do
  symbol "trueo"
  return $ C "true" []

falseo :: Parser (Term X)
falseo = do
  symbol "falseo"
  return $ C "false" []


parseDesugarTerm :: Parser (Term X)
parseDesugarTerm = try c <|> v
  where
    v = V <$> ident
    c = angleBr $ do
      name  <- ident
      symbol ":"
      terms <- many parseDesugarTerm
      return $ C name terms


-- goal
parseFresh :: Parser (G X)
parseFresh = boxBr $ do
  names <- some ident
  symbol ":"
  goal <- parseGoal
  return $ foldr (\name acc -> Fresh name acc) goal names

parseInvoke :: Parser (G X)
parseInvoke = curvyBr $
      Invoke
  <$> ident
  <*> many parseTerm

parsePat :: Parser (G X)
parsePat = try $ do
      term1 <- parseTerm
      symbol "==="
      term2 <- parseTerm
      return $ term1 :=: term2
  <|> try (roundBr parseOp)
  <|> try parseFresh
  <|> parseInvoke

parseOp :: Parser (G X)
parseOp = makeExprParser parsePat ops
  where
    go assoc op f = assoc (f <$ symbol op)
    ops = [ [ go InfixR "/\\" (:/\:) ]
          , [ go InfixR "\\/" (:\/:) ]
          ]

parseGoal :: Parser (G X)
parseGoal = try parseConde
  <|> parseDesugarGoal

parseConde :: Parser (G X)
parseConde = do
  symbol "conde"
  goals <- some (roundBr parseDesugarGoal)
  return $ foldr1 (\goal acc -> goal :\/: acc) goals

parseDesugarGoal :: Parser (G X)
parseDesugarGoal = sc
   *> try parseOp
  <|> try parseFresh
  <|> parseInvoke
  <|> parseLet


-- definition
parseLetDef :: Parser (G X -> G X)
parseLetDef = do
  symbol "::"
  name <- ident
  args <- many ident
  symbol "="
  goal <- parseGoal
  return $ Let (name, args, goal)


-- let
parseLet :: Parser (G X)
parseLet = curvyBr $ do
  letDef  <- parseLetDef
  symbol "$"
  goal <- curvyBr $ try parseGoal <|> roundBr parseGoal
  return $ letDef goal


-- program
parseProg :: Parser (G X -> G X)
parseProg = do
  defs <- some parseLetDef
  return $ \mainGoal -> foldr (\def acc -> def $ acc) mainGoal defs

parseDefs :: Parser [G X -> G X]
parseDefs = some parseLetDef

