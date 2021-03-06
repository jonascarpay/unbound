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
import Data.Map (Map)
import qualified Data.Map as M
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

data Exp v
  = Var v
  | App (Exp v) (Exp v)
  | Lam (Scope () Exp v)
  | Acc (Exp v) String
  | Att (Map String (Scope String Exp v))
  | Int Int
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Applicative Exp where
  pure = Var
  (<*>) = ap

instance Monad Exp where
  Var a >>= f = f a
  App l r >>= f = App (l >>= f) (r >>= f)
  Lam b >>= f = Lam $ b >>= lift . f
  Att b >>= f = Att $ (>>= lift . f) <$> b
  Int n >>= _ = Int n
  Acc b n >>= f = Acc (b >>= f) n

data ValueF f
  = VClosure (Scope () Exp Void)
  | VInt Int
  | VAtt (Map String f)

lam :: Eq a => a -> Exp a -> Exp a
lam arg = Lam . abstract1 arg

recAttr :: Ord a => [(a, Exp a)] -> Map a (Scope a Exp a)
recAttr binds = abstract (\a -> if M.member a m then Just a else Nothing) <$> m
  where
    m = M.fromList binds

instantiateAttr :: forall a b. Ord a => Map a (Scope a Exp b) -> a -> Maybe (Exp b)
instantiateAttr m a = instantiate unsafe <$> M.lookup a m
  where
    unsafe :: a -> Exp b
    unsafe = instantiate unsafe . fromJust . flip M.lookup m

nines :: Bool
nines =
  all
    ((== Right (Int 9)) . fmap whnf . parse)
    [ "9",
      "(x: x) 9",
      "(a: b: a) 9 10",
      "{ b = 9, a = b, c = a }.c",
      "{ b = {a = 9}, c = b, d = c.a }.d",
      "(a: b: a) 9 ((x: x x) (x: x x))",
      "{ y = f: (x: f (x x)) (x: f (x x)) \
      \, b = y (self: { nine = 9, value = self.nine }) \
      \}.b.value"
    ]

whnf :: Exp Void -> Exp Void
whnf (Var a) = absurd a
whnf (Lam a) = Lam a
whnf (Int n) = Int n
whnf (Att m) = Att m
whnf (Acc b n) = case whnf b of
  Att m | Just r <- instantiateAttr m n -> whnf r
  _ -> Acc b n
whnf (App f x) = case whnf f of
  Lam b -> whnf $ instantiate1 x b
  r -> App f r

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

pTerm1 :: ReadP (Exp String)
pTerm1 = do
  h <- pTerm0
  foldl Acc h <$> many (string "." *> pName)

pTerm0 :: ReadP (Exp String)
pTerm0 =
  R.choice
    [ Var <$> pName,
      Int <$> number,
      fmap (Att . recAttr) $ string "{" *> R.sepBy (liftA2 (,) (pName <* string "=") pExp) (string ",") <* string "}",
      string "(" *> pExp <* string ")"
    ]

pExp :: ReadP (Exp String)
pExp =
  R.choice
    [ foldl1 App <$> R.many1 pTerm1,
      liftA2 lam (pName <* string ":") pExp
    ]
