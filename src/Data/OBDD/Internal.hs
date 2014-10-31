module Data.OBDD.Internal where

import Prelude hiding (not, and, or, const)
import qualified Prelude
import qualified Data.Map as Map
import Control.Applicative

newtype Var = Var Int
  deriving (Eq, Ord, Show)

type Binding = Map.Map Var Bool

data OBDD
  = Branch Var OBDD OBDD
  | Leaf Bool
  deriving Eq

instance Show OBDD where
    show (Leaf True)            = "T"
    show (Leaf False)           = "F"
    show (Branch (Var i) lo hi) = "Branch " ++ show i
                                    ++ " (" ++ show lo ++ ")"
                                    ++ " (" ++ show hi ++ ")"

apply :: (Bool -> Bool -> Bool) -> OBDD -> OBDD -> OBDD
apply f = go
  where
    go x@(Branch xi xlo xhi) y@(Branch yi ylo yhi)
      | xi < yi              = reduce xi (go xlo y)   (go xhi y)
      | yi < xi              = reduce yi (go x   ylo) (go x   yhi)
      | otherwise            = reduce xi (go xlo ylo) (go xhi yhi)
    go x (Branch yi ylo yhi) = reduce yi (go x   ylo) (go x   yhi)
    go (Branch xi xlo xhi) y = reduce xi (go xlo y)   (go xhi y)
    go (Leaf x) (Leaf y)   = Leaf $ f x y

reduce :: Var -> OBDD -> OBDD -> OBDD
reduce i lo hi
  | lo == hi  = lo
  | otherwise = Branch i lo hi

const :: Bool -> OBDD
const = Leaf

true :: OBDD
true = const True

false :: OBDD
false = const False

var :: Int -> OBDD
var x = Branch (Var x) (Leaf False) (Leaf True)

not :: OBDD -> OBDD
not = xor (const True)

and :: OBDD -> OBDD -> OBDD
and = apply (&&)

or :: OBDD -> OBDD -> OBDD
or = apply (||)

xor :: OBDD -> OBDD -> OBDD
xor = apply (/=)

iff :: OBDD -> OBDD -> OBDD
iff = apply (==)

impl :: OBDD -> OBDD -> OBDD
impl = apply (\x y -> Prelude.not x || y)

restrict :: Var -> Bool -> OBDD -> OBDD
restrict i v = go
  where
    go x@(Branch xi lo hi)
      | xi < i    = reduce xi (go lo) (go hi)
      | xi > i    = x
      | otherwise = if v then hi else lo
    go x = x

exists :: Var -> OBDD -> Bool
exists i x = isTautology $ restrict i True x `or` restrict i False x

forall :: Var -> OBDD -> Bool
forall i x = isTautology $ restrict i True x `and` restrict i False x

isTautology :: OBDD -> Bool
isTautology = (== (const True))

isContradiction :: OBDD -> Bool
isContradiction = (== (const False))

evaluate :: Binding -> OBDD -> Bool
evaluate env = go
  where
    go (Leaf x) = x
    go (Branch i lo hi) = case Map.lookup i env of
      Just False -> go lo
      Just True  -> go hi
      Nothing    -> error "evaluate: incorrect binding"

anySat :: OBDD -> Maybe Binding
anySat (Leaf False)     = Nothing
anySat (Leaf True)      = Just Map.empty
anySat (Branch i lo hi) = Map.insert i False <$> anySat lo
                      <|> Map.insert i True  <$> anySat hi

allSat :: OBDD -> [Binding]
allSat (Leaf False)     = []
allSat (Leaf True)      = [Map.empty]
allSat (Branch i lo hi) = map (Map.insert i False) (allSat lo)
                       ++ map (Map.insert i True)  (allSat hi)
