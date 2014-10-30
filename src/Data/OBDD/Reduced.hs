module Data.OBDD.Reduced
  ( ROBDD(..)
  , Var
  , true
  , false
  , var
  , not
  , and
  , or
  , impl
  , iff
  ) where

import Prelude hiding (not, and, or)

newtype Var = Var Int

data ROBDD = ROBDD
  deriving (Eq, Show)

true  :: ROBDD
false :: ROBDD
var   :: Int -> ROBDD
not   :: ROBDD -> ROBDD
and   :: ROBDD -> ROBDD -> ROBDD
or    :: ROBDD -> ROBDD -> ROBDD
impl  :: ROBDD -> ROBDD -> ROBDD
iff   :: ROBDD -> ROBDD -> ROBDD
true  = undefined
false = undefined
var   = undefined
not   = undefined
and   = undefined
or    = undefined
impl  = undefined
iff   = undefined
