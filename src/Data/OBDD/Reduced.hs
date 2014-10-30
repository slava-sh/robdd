module Data.OBDD.Reduced
  ( ROBDD(..)
  , Var(..)
  , constant
  , var
  , not
  , and
  , or
  , xor
  , impl
  , iff
  ) where

import Prelude hiding (not, and, or)
import qualified Prelude

newtype Var = Var Int
  deriving (Eq, Ord, Show)

data ROBDD
  = Branch Var ROBDD ROBDD
  | Leaf Bool
  deriving Eq

instance Show ROBDD where
    show (Leaf True)            = "T"
    show (Leaf False)           = "F"
    show (Branch (Var i) lo hi) = "Branch " ++ show i
                                    ++ " (" ++ show lo ++ ")"
                                    ++ " (" ++ show hi ++ ")"

apply :: (Bool -> Bool -> Bool) -> ROBDD -> ROBDD -> ROBDD
apply f = go
  where
    go x@(Branch xi xa xb) y@(Branch yi ya yb)
      | xi < yi            = reduce xi (go xa y)  (go xb y)
      | yi < xi            = reduce yi (go x  ya) (go x  yb)
      | otherwise          = reduce xi (go xa ya) (go xb yb)
    go x (Branch yi ya yb) = reduce yi (go x  ya) (go x  yb)
    go (Branch xi xa xb) y = reduce xi (go xa y)  (go xb y)
    go (Leaf x) (Leaf y)   = Leaf $ f x y

    reduce i lo hi
      | lo == hi  = lo
      | otherwise = Branch i lo hi

constant :: Bool -> ROBDD
constant = Leaf

var :: Int -> ROBDD
var x = Branch (Var x) (Leaf False) (Leaf True)

not :: ROBDD -> ROBDD
not = xor (constant True)

and :: ROBDD -> ROBDD -> ROBDD
and = apply (&&)

or :: ROBDD -> ROBDD -> ROBDD
or = apply (||)

xor :: ROBDD -> ROBDD -> ROBDD
xor = apply (/=)

iff :: ROBDD -> ROBDD -> ROBDD
iff = apply (==)

impl :: ROBDD -> ROBDD -> ROBDD
impl = apply (\x y -> Prelude.not x || y)
