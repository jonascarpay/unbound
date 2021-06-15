{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module M04_Bound where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.Maybe
import Data.Void
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as R

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

instantiate1 :: Monad f => f a -> Scope () f a -> f a
instantiate1 = instantiate . const

closed :: Traversable f => f a -> Either a (f b)
closed = traverse Left

isClosed :: Foldable f => f a -> Bool
isClosed = null

data ValueF f
  = VInt Int
  | VList [f]
  | VClosure (Scope () Exp Void)
  deriving (Show, Functor, Foldable, Traversable)

newtype Fix f = Fix (f (Fix f))

type LazyValue = ValueF Int

type Value = Fix ValueF

data Exp v
  = Var v
  | App (Exp v) (Exp v)
  | Lam (Scope () Exp v)
  | Val (ValueF (Exp v))
  deriving (Show, Functor, Foldable, Traversable)

instance Applicative Exp where
  pure = Var
  (<*>) = ap

instance Monad Exp where
  Var a >>= f = f a
  App l r >>= f = App (l >>= f) (r >>= f)
  Lam b >>= f = Lam $ b >>= lift . f
  Val v >>= f = Val $ (>>= f) <$> v

lam :: Eq a => a -> Exp a -> Exp a
lam arg = Lam . abstract1 arg

app :: Exp a -> Exp a -> Exp a
app arg (Lam body) = instantiate1 arg body
app _ _ = error "not a closure"

parse :: String -> Either String (Exp Void)
parse str = case R.readP_to_S (pExp <* R.eof) str of
  [] -> Left "no parse"
  [(e, _)] -> case closed e of
    Left var -> Left $ "unbound variable " <> var
    Right a -> pure a
  _ -> Left "ambiguous"

pSpace :: ReadP ()
pSpace = void $ R.many $ R.satisfy isSpace

lexeme :: ReadP a -> ReadP a
lexeme = (<* pSpace)

pName :: ReadP String
pName = lexeme $ do
  h <- R.satisfy isLower
  t <- R.munch isAlpha
  pure $ h : t

string :: String -> ReadP ()
string = void . lexeme . R.string

number :: ReadP Int
number = lexeme $ read <$> R.munch1 isNumber

pTerm :: ReadP (Exp String)
pTerm =
  R.choice
    [ Var <$> pName,
      Val . VInt <$> number,
      string "(" *> pExp <* string ")"
    ]

pExp :: ReadP (Exp String)
pExp =
  R.choice
    [ App <$> pTerm <*> pTerm,
      pTerm,
      liftA2 lam (pName <* string ":") pExp
    ]
