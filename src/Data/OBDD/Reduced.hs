module Data.OBDD.Reduced
  ( ROBDD(..)
  , Var(..)
  , Binding
  , constant
  , var
  , not
  , and
  , or
  , xor
  , impl
  , iff
  , isTautology
  , isContradiction
  , restrict
  , exists
  , forall
  , anySat
  , allSat
  , evaluate
  ) where

import Prelude hiding (not, and, or)
import qualified Prelude
import qualified Data.Map as Binding
import Control.Applicative

newtype Var = Var Int
  deriving (Eq, Ord, Show)

type Binding = Binding.Map Var Bool

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
    go x@(Branch xi xlo xhi) y@(Branch yi ylo yhi)
      | xi < yi              = reduce xi (go xlo y)   (go xhi y)
      | yi < xi              = reduce yi (go x   ylo) (go x   yhi)
      | otherwise            = reduce xi (go xlo ylo) (go xhi yhi)
    go x (Branch yi ylo yhi) = reduce yi (go x   ylo) (go x   yhi)
    go (Branch xi xlo xhi) y = reduce xi (go xlo y)   (go xhi y)
    go (Leaf x) (Leaf y)   = Leaf $ f x y

reduce :: Var -> ROBDD -> ROBDD -> ROBDD
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

restrict :: Var -> Bool -> ROBDD -> ROBDD
restrict i v = go
  where
    go x@(Branch xi lo hi)
      | xi < i    = reduce xi (go lo) (go hi)
      | xi > i    = x
      | otherwise = if v then hi else lo
    go x = x

exists :: Var -> ROBDD -> Bool
exists i x = isTautology $ restrict i True x `or` restrict i False x

forall :: Var -> ROBDD -> Bool
forall i x = isTautology $ restrict i True x `and` restrict i False x

isTautology :: ROBDD -> Bool
isTautology = (== (constant True))

isContradiction :: ROBDD -> Bool
isContradiction = (== (constant False))

evaluate :: Binding -> ROBDD -> Bool
evaluate env = go
  where
    go (Leaf x) = x
    go (Branch i lo hi) = case Binding.lookup i env of
      Just False -> go lo
      Just True  -> go hi
      Nothing    -> error "evaluate: incorrect binding"

anySat :: ROBDD -> Maybe Binding
anySat (Leaf False)     = Nothing
anySat (Leaf True)      = Just Binding.empty
anySat (Branch i lo hi) = Binding.insert i False <$> anySat lo
                      <|> Binding.insert i True  <$> anySat hi

allSat :: ROBDD -> [Binding]
allSat (Leaf False)     = []
allSat (Leaf True)      = [Binding.empty]
allSat (Branch i lo hi) = map (Binding.insert i False) (allSat lo)
                       ++ map (Binding.insert i True)  (allSat hi)
