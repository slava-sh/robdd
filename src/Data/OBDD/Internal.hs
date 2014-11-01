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

type IdPool = Map (Var, Id, Id) Id

newtype IdPoolM a = IdPoolM (State IdPool a)
  deriving (Functor, Applicative, Monad, MonadState IdPool)

runIdPoolM :: IdPoolM a -> a
runIdPoolM (IdPoolM s) = evalState s Map.empty

type RefPool = Map Id ROBDD

type RefPoolM a = StateT RefPool IdPoolM a

type Binding = Map Var Bool

data BranchType = Orig | Ref
  deriving (Eq, Ord, Show)

data ROBDD
  = Branch BranchType Id Var ROBDD ROBDD
  | Leaf Bool
  deriving Eq

getId :: ROBDD -> Id
getId (Branch _ i _ _ _) = i
getId (Leaf False)       = Id 0
getId (Leaf True)        = Id 1

instance Show Id where
  show (Id i) = "#" ++ show i

instance Show ROBDD where
  show (Branch t i (Var v) lo hi) = showType t ++ show i ++ " " ++ show v
                                     ++ " (" ++ show lo ++ ")"
                                     ++ " (" ++ show hi ++ ")"
    where
      showType Orig = "Branch"
      showType Ref  = "Ref"
  show (Leaf True)                = "T"
  show (Leaf False)               = "F"

const :: Bool -> ROBDD
const = Leaf

varM :: Int -> IdPoolM ROBDD
varM x = evalStateT (branch (Var x) (Leaf False) (Leaf True)) Map.empty

apply :: (Bool -> Bool -> Bool) -> ROBDD -> ROBDD -> IdPoolM ROBDD
apply f a b = evalStateT (go a b) Map.empty
  where
    go :: ROBDD -> ROBDD -> RefPoolM ROBDD
    go x@(Branch _ _ xv xlo xhi) y@(Branch _ _ yv ylo yhi)
      | xv < yv                  = branch' xv (go xlo y)   (go xhi y)
      | yv < xv                  = branch' yv (go x   ylo) (go x   yhi)
      | otherwise                = branch' xv (go xlo ylo) (go xhi yhi)
    go x (Branch _ _ yv ylo yhi) = branch' yv (go x   ylo) (go x   yhi)
    go (Branch _ _ xv xlo xhi) y = branch' xv (go xlo y)   (go xhi y)
    go (Leaf x) (Leaf y)         = return . Leaf $ f x y

    branch' v loM hiM = do
      lo <- loM
      hi <- hiM
      branch v lo hi

nextId :: IdPoolM Id
nextId = do
  pool <- get
  return . Id $ 2 + Map.size pool

branch :: Var -> ROBDD -> ROBDD -> RefPoolM ROBDD
branch v lo hi
  | equals lo hi = return lo
  | otherwise    = do
      idPool <- lift get
      case Map.lookup key idPool of
        Just i  -> get >>= maybe (newBranch i) return . Map.lookup i
        Nothing -> lift nextId >>= newBranch
  where
    key = (v, getId lo, getId hi)

    newBranch i = do
      lift . modify $ Map.insert key i
      let origBranch = Branch Orig i v lo hi
          refBranch  = Branch Ref  i v lo hi
      modify $ Map.insert i refBranch
      return origBranch

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
    -- go x@(Branch _ xi lo hi)
    --   | xi < i    = branch xi (go lo) (go hi)
    --   | xi > i    = x
    --   | otherwise = if v then hi else lo
    -- go x = x

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
fold f z0 z1 = undefined
  where
    go (Leaf False)     = z0
    go (Leaf True)      = z1
    go (Branch _ _ i lo hi) = f i (go lo) (go hi)

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
