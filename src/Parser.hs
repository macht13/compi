module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax
{-
binary s f assoc = Ex.Infix  (reservedOp s >> return (BinOp f)) assoc

table = [[binary "*" Times Ex.AssocLeft,
          binary "/" Divide Ex.AssocLeft]
        ,[binary "+" Plus Ex.AssocLeft,
          binary "-" Minus Ex.AssocLeft]]
-}
int :: Parser Term
int = do
  n <- num
  return $ NumT $ n
{-
expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try extern
      <|> try function
      <|> try call
      <|> variable
      <|> parens expr
-}{-
contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

program :: Parser Program
program = do
  funcdefs <- sepEndBy funcdef (char ';')
  return $ Program funcdefs

funcdef :: Parser Funcdef
funcdef = do
  idF <- identifier
  parsF <- parens pars
  statsF <- stats
  reserved "end"
  return $ Funcdef idF parsF statsF

pars :: Parser Pars
pars = commaSep identifier

stats :: Parser Stats
stats = sepEndBy stat (char ';')

stat :: Parser Stat
stat = (try returnStat)
   <|> condStat
   <|> varDefStat
   <|> varAsgnStat

returnStat :: Parser Stat
returnStat = do
  reserved "return"
  exprVal <- expr
  return $ Return exprVal

condStat :: Parser Stat
condStat = do
  idC <- optionMaybe (identifier <* (char ':'))
  _   <- reserved "cond"
  gC  <- sepEndBy (((try guardedC) <|> guardedB) (char ';'))
  _   <- reserved "end"
  return $ CondStat idC gC

varDefStat :: Parser Stat
varDefStat = do
  _   <- reserved "var"
  idD <- identifier
  _   <- reservedOp "="
  exD <- expr
  return $ Vardef idD exD

varAsgnStat :: Parser Stat
varAsgnStat = do
  idA <- identifier
  _   <- reservedOp "="
  exA <- expr
  return $ Varasgn idA exA

guardedC :: Parser Guarded
guardedC = do
  exprC  <- optionMaybe (expr)
  _      <- reservedOp "->"
  statsG <- stats
  _      <- reserved "continue"
  idC    <- optionMaybe (identifier)
  return $ exprC statsG (Left Continue) idC

guardedB :: Parser Guarded
guardedB = do
  exprC    <- optionMaybe (expr)
  _      <- reservedOp "->"
  statsG <- stats
  _      <- reserved "break"
  idC    <- optionMaybe (identifier)
  return $ exprC statsG (Left Continue) idC

expr :: Parser Expr
expr = (try termE)
  <|> (try preOp)
  <|> (try binOp)
  <|> (try boolOp)

termE :: Parser Expr
termE = do
  t <- term
  return $ TermE t

preOp :: Parser Expr
preOp = (reservedOp "not" *> )
-}
{-
data Term =
  ParenExpr Expr
  | NumT    Var
  | VarUsg  Id
  | FnCall  Id [Expr]
    deriving (Eq, Ord, Show)

data TeOp
  = Not
  | HeadOp
  | TailOp
  | Islist
    deriving (Eq, Ord, Show)

data BiOp
  = Plus
  | Times
  | Or
  | Dot
    deriving (Eq, Ord, Show)

data BoOp
  = Geq
  | Equ
  | Minus
    deriving (Eq, Ord, Show)
-}

{-}
parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseProgram :: String -> Either ParseError Program
parseProgram s = parse (contents program) "<stdin>" s
-}