module Syntax where

type Id = String
type Par = Id
type Pars = [Par]
type Stats = [Stat]

{-
  variable is a 64 bit value,
  checked at runtime whether 0bit is:
  0 -> rest >> 1 is 63 bit signed integer
  1 -> rest - 1 ist 64 bit address
-}
type Var = Integer

data Program
  = Program [Funcdef]
    deriving (Eq, Ord, Show)

data Funcdef
  = Funcdef Id Pars Stats
    deriving (Eq, Ord, Show)

data Stat
  = Return    Expr
  | CondStat  (Maybe Id) [Guarded]
  | Vardef    Id Expr
  | Varasgn   Id Expr
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
  | TermOp [PreOp] Term
  | BinOp BiOp [Term]
  | BoolOp BoOp Term Term
    deriving (Eq, Ord, Show)

data Term =
  ParenExpr Expr
  | NumT    Var
  | VarUsg  Id
  | FnCall  Id [Expr]
    deriving (Eq, Ord, Show)

data PreOp
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