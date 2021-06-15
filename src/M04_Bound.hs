{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module M04_Bound where

import Control.Monad
import Control.Monad.Trans

data Var b a = Bound b | Free a
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative (Var b) where
  pure = Free
  (<*>) = ap

instance Monad (Var b) where
  Free a >>= f = f a
  Bound b >>= _ = Bound b

newtype Scope b f a = Scope {unScope :: f (Var b (f a))}
  deriving (Functor, Foldable, Traversable)

deriving instance Show (f (Var b (f a))) => Show (Scope b f a)

deriving instance Eq (f (Var b (f a))) => Eq (Scope b f a)

instance Monad f => Applicative (Scope b f) where
  pure = Scope . pure . pure . pure
  (<*>) = ap

instance Monad f => Monad (Scope b f) where
  (Scope sa) >>= f =
    Scope $
      sa >>= \case
        Free a -> a >>= unScope . f
        Bound b -> pure $ Bound b

instance MonadTrans (Scope b) where
  lift = Scope . pure . Free

abstract :: forall b f a. Monad f => (a -> Maybe b) -> f a -> Scope b f a
abstract f m = Scope $ go <$> m
  where
    go :: a -> Var b (f a)
    go a = case f a of
      Just b -> Bound b
      Nothing -> Free (pure a)

abstract1 :: (Monad f, Eq a) => a -> f a -> Scope () f a
abstract1 a = abstract $ \v -> if a == v then Just () else Nothing

instantiate :: Monad f => (b -> f a) -> Scope b f a -> f a
instantiate f (Scope m) =
  m >>= \case
    Free a -> a
    Bound b -> f b

closed :: Traversable f => f a -> Maybe (f b)
closed = traverse (const Nothing)

isClosed :: Foldable f => f a -> Bool
isClosed = null

data Exp v
  = Var v
  | App (Exp v) (Exp v)
  | Lam (Scope () Exp v)
  | Int Int
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative Exp where
  pure = Var
  (<*>) = ap

instance Monad Exp where
  Var a >>= f = f a
  App l r >>= f = App (l >>= f) (r >>= f)
  Lam b >>= f = Lam $ b >>= lift . f
  Int n >>= _ = Int n
