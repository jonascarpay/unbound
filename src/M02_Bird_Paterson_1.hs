{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

module M02_Bird_Paterson_1 where

import Control.Monad

data Exp v
  = Var v
  | App (Exp v) (Exp v)
  | Lam (Exp (Maybe v))
  | Int Int
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative Exp where
  pure = Var
  (<*>) = ap

instance Monad Exp where
  Var a >>= f = f a
  App l r >>= f = App (l >>= f) (r >>= f)
  Lam b >>= f = Lam (b >>= traverse f)
  Int n >>= _ = Int n

abstract :: (Functor f, Eq a) => a -> f a -> f (Maybe a)
abstract a = fmap go
  where
    go x = if x == a then Nothing else Just x

instantiate :: Exp a -> Exp (Maybe a) -> Exp a
instantiate sub body =
  body >>= \case
    Nothing -> sub
    Just x -> Var x

closed :: Exp a -> Maybe (Exp b)
closed = traverse (const Nothing)

isClosed :: Exp a -> Bool
isClosed = null
