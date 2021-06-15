{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

module M03_Bird_Paterson_2 where

import Control.Monad

data Exp v
  = Var v
  | App (Exp v) (Exp v)
  | Lam (Exp (Maybe (Exp v)))
  | Int Int
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative Exp where
  pure = Var
  (<*>) = ap

instance Monad Exp where
  Var a >>= f = f a
  App l r >>= f = App (l >>= f) (r >>= f)
  Lam b >>= f =
    Lam $
      b >>= \case
        Nothing -> pure Nothing
        Just a -> pure . f <$> a
  Int n >>= _ = Int n

abstract :: (Applicative f, Eq a) => a -> f a -> f (Maybe (f a))
abstract a = fmap go
  where
    go x = if x == a then Nothing else Just (pure x)

instantiate :: Exp a -> Exp (Maybe (Exp a)) -> Exp a
instantiate sub body =
  body >>= \case
    Nothing -> sub
    Just a -> a

-- closed :: Exp a -> Maybe (Exp b)
-- closed = traverse (const Nothing)

-- isClosed :: Exp a -> Bool
-- isClosed = null
