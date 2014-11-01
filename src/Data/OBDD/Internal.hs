{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.OBDD.Internal where

import Prelude hiding (not, and, or, const)
import qualified Prelude
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad.State

newtype Id = Id Int
  deriving (Eq, Ord)

newtype Var = Var Int
  deriving (Eq, Ord, Show)

type RefPool = Map Id ROBDD

type IdPool = Map (Var, Id, Id) Id

newtype IdPoolM a = IdPoolM (State IdPool a)
  deriving (Functor, Applicative, Monad, MonadState IdPool)

runIdPoolM :: IdPoolM a -> a
runIdPoolM (IdPoolM s) = evalState s Map.empty

type Binding = Map Var Bool

data ROBDD
  = Branch Id Var ROBDD ROBDD
  | Ref Id
  | Leaf Bool
  deriving Eq

getId :: ROBDD -> Id
getId (Branch i _ _ _) = i
getId (Ref i)          = i
getId (Leaf False)     = Id 0
getId (Leaf True)      = Id 1

instance Show Id where
  show (Id i) = "#" ++ show i

instance Show ROBDD where
  show (Branch i (Var v) lo hi) = "Branch" ++ show i ++ " " ++ show v
                                   ++ " (" ++ show lo ++ ")"
                                   ++ " (" ++ show hi ++ ")"
  show (Ref i)                  = "Ref" ++ show i
  show (Leaf True)              = "T"
  show (Leaf False)             = "F"

apply :: (Bool -> Bool -> Bool) -> ROBDD -> ROBDD -> IdPoolM ROBDD
apply f = go
  where
    go = undefined
    -- go x@(Branch xi xlo xhi) y@(Branch yi ylo yhi)
    --   | xi < yi              = branch xi (go xlo y)   (go xhi y)
    --   | yi < xi              = branch yi (go x   ylo) (go x   yhi)
    --   | otherwise            = branch xi (go xlo ylo) (go xhi yhi)
    -- go x (Branch yi ylo yhi) = branch yi (go x   ylo) (go x   yhi)
    -- go (Branch xi xlo xhi) y = branch xi (go xlo y)   (go xhi y)
    -- go (Leaf x) (Leaf y)   = Leaf $ f x y

    -- branch i lo hi
    --   | lo == hi  = lo
    --   | otherwise = Branch i lo hi

branch = undefined

const :: Bool -> ROBDD
const = Leaf

varM :: Int -> IdPoolM ROBDD
varM x = branch (Var x) (Leaf False) (Leaf True)

notM :: ROBDD -> IdPoolM ROBDD
notM = xorM (const True)

andM :: ROBDD -> ROBDD -> IdPoolM ROBDD
andM = apply (&&)

orM :: ROBDD -> ROBDD -> IdPoolM ROBDD
orM = apply (||)

xorM :: ROBDD -> ROBDD -> IdPoolM ROBDD
xorM = apply (/=)

iffM :: ROBDD -> ROBDD -> IdPoolM ROBDD
iffM = apply (==)

implM :: ROBDD -> ROBDD -> IdPoolM ROBDD
implM = apply (\x y -> Prelude.not x || y)

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

reduce :: Expr -> IdPoolM ROBDD
reduce (EConst x)   = return $ const x
reduce (EVar   x)   = varM x
reduce (ENot   x)   = reduce x >>= notM
reduce (EAnd   x y) = binary andM  x y
reduce (EOr    x y) = binary orM   x y
reduce (EXor   x y) = binary xorM  x y
reduce (EIff   x y) = binary iffM  x y
reduce (EImpl  x y) = binary implM x y

binary :: (ROBDD -> ROBDD -> IdPoolM ROBDD) -> Expr -> Expr -> IdPoolM ROBDD
binary f x y = do
  x' <- reduce x
  y' <- reduce y
  f x' y'

restrict :: Var -> Bool -> ROBDD -> IdPoolM ROBDD
restrict i v = undefined
  where
    go x@(Branch _ xi lo hi)
      | xi < i    = branch xi (go lo) (go hi)
      | xi > i    = x
      | otherwise = if v then hi else lo
    go x = x

-- exists :: Var -> ROBDD -> IdPoolM Bool
-- exists i x = isTautology $ restrict i True x `or` restrict i False x
-- 
-- forall :: Var -> ROBDD -> IdPoolM Bool
-- forall i x = isTautology $ restrict i True x `and` restrict i False x

equals :: ROBDD -> ROBDD -> Bool
equals x y = getId x == getId y

isTautology :: ROBDD -> Bool
isTautology = (`equals` (const True))

isContradiction :: ROBDD -> Bool
isContradiction = (`equals` (const False))

fold :: (Var -> a -> a -> a) -> a -> a -> ROBDD -> a
fold f z0 z1 = go
  where
    go (Leaf False)     = z0
    go (Leaf True)      = z1
    go (Branch _ i lo hi) = f i (go lo) (go hi)
    go _ = undefined

evaluate :: Binding -> ROBDD -> Bool
evaluate env = fold f False True
  where
    f i lo hi = case Map.lookup i env of
      Just False -> lo
      Just True  -> hi
      Nothing    -> error "evaluate: incorrect binding"

anySat :: ROBDD -> Maybe Binding
anySat = fold f Nothing (Just Map.empty)
  where
    f i lo hi = Map.insert i False <$> lo
            <|> Map.insert i True  <$> hi

allSat :: ROBDD -> [Binding]
allSat = fold f [] [Map.empty]
  where
    f i lo hi = map (Map.insert i False) lo
             ++ map (Map.insert i True)  hi
