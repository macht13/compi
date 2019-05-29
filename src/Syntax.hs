module Syntax where

type Id = String
type Par = Id
type Pars = [Par]
type Stats = [Stat]

data Program
  = Program [Funcdef]
    deriving (Eq, Ord, Show)

data Funcdef
  = Funcdef Id Pars Stats
    deriving (Eq, Ord, Show)

data Stat
  = Return    Expr
  | CondStat  Cond
  | Vardef    Id Expr
  | Varasgn   Id Expr
    deriving (Eq, Ord, Show)

data Cond
  = Cond (Maybe Id) [Guarded]
    deriving (Eq, Ord, Show)

data Guarded
  = Guarded  (Maybe Expr) Stats (Either Continue Break) (Maybe Id)
    deriving (Eq, Ord, Show)

data Continue
  = Continue
    deriving (Eq, Ord, Show)

data Break
  = Break
    deriving (Eq, Ord, Show)

data Expr =
  TermE Term
  | TermOp [TeOp] Term
  | BinOp BiOp Term [Term]
  | BoolOp BoOp Term Term
    deriving (Eq, Ord, Show)

data Term =
  ParenExpr Expr
  | NumT    Int
  | VarUsg  Id
  | FnCall  Id [Expr]
    deriving (Eq, Ord, Show)

data TeOp
  = Not
  | Head
  | Tail
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