{-# LANGUAGE DeriveTraversable #-}

module Naive where

import Data.List
import Data.Set (Set)
import qualified Data.Set as S

type Name = String

data Exp v
  = Free v
  | Bound !Int
  | App (Exp v) (Exp v)
  | Lam (Exp v)
  deriving (Eq, Show, Functor, Foldable, Traversable)

abstract :: a -> Exp a -> Exp a
abstract = undefined

instantiate :: Exp a -> Exp a -> Exp a
instantiate = undefined
