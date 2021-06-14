{-# LANGUAGE DeriveTraversable #-}

module M01_DBI where

data Exp v
  = Free v
  | Bound !Int
  | App (Exp v) (Exp v)
  | Lam (Exp v)
  deriving (Eq, Show, Functor, Foldable, Traversable)

abstract :: Eq a => a -> Exp a -> Exp a
abstract x = go 0
  where
    go lvl (Free var)
      | var == x = Bound lvl
      | otherwise = Free var
    go _ (Bound i) = Bound i
    go lvl (App fn arg) = App (go lvl fn) (go lvl arg)
    go lvl (Lam body) = Lam (go (succ lvl) body)

instantiate :: Exp a -> Exp a -> Exp a
instantiate arg = go 0
  where
    go lvl (Bound n)
      | n == lvl = arg
      | otherwise = Bound n
    go _ (Free var) = Free var
    go lvl (App f x) = App (go lvl f) (go lvl x)
    go lvl (Lam body) = Lam (go (succ lvl) body)

illegal :: Exp a
illegal = Lam (Bound 2)

closed :: Exp a -> Maybe (Exp b)
closed = traverse (const Nothing)

isClosed :: Exp a -> Bool
isClosed = null
