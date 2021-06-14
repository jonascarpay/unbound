{-# LANGUAGE DeriveTraversable #-}

module M02_Bird_Paterson_1 where

data Exp v
  = Var v
  | App (Exp v) (Exp v)
  | Lam (Exp (Maybe v))
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative Exp where
  pure = Var

instance Monad Exp where
  Var a >>= f = f a
  App l r >>= f = App (l >>= f) (r >>= f)
  Lam b >>= f = Lam (b >>= traverse f)

abstract' :: (Functor f, Eq a) => a -> f a -> f (Maybe a)
abstract' a = fmap go
  where
    go x = if x == a then Nothing else Just x

-- instantiate :: Exp a -> Exp a -> Exp a
-- instantiate arg = go 0
--   where
--     go lvl (Bound n)
--       | n == lvl = arg
--       | otherwise = Bound n
--     go _ (Free var) = Free var
--     go lvl (App f x) = App (go lvl f) (go lvl x)
--     go lvl (Lam body) = Lam (go (succ lvl) body)

-- illegal :: Exp a
-- illegal = Lam (Bound 2)

-- closed :: Exp a -> Maybe (Exp b)
-- closed = traverse (const Nothing)

-- isClosed :: Exp a -> Bool
-- isClosed = null
