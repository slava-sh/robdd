module Data.OBDD.Reduced.Internal.Expr where

import Prelude hiding (not, and, or, const)

data Expr
  = EConst Bool
  | EVar   Int
  | ENot   Expr
  | EAnd   Expr Expr
  | EOr    Expr Expr
  | EXor   Expr Expr
  | EIff   Expr Expr
  | EImpl  Expr Expr
  deriving (Eq, Show)

const :: Bool -> Expr
const = EConst

true :: Expr
true = EConst True

false :: Expr
false = EConst False

var :: Int -> Expr
var = EVar

not :: Expr -> Expr
not = ENot

and :: Expr -> Expr -> Expr
and = EAnd

or :: Expr -> Expr -> Expr
or = EOr

xor :: Expr -> Expr -> Expr
xor = EXor

iff :: Expr -> Expr -> Expr
iff = EIff

impl :: Expr -> Expr -> Expr
impl = EImpl
