{-# LANGUAGE DeriveFunctor #-}

module LLC where

import Control.Applicative
import Control.Monad
import Data.Char
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as R

data Exp a
  = Var a
  | App (Exp a) (Exp a)
  | Lam (Exp (Maybe a))
  deriving (Eq, Show, Functor)

instance Applicative Exp where
  pure = Var
  (<*>) = ap

instance Monad Exp where
  Var a >>= f = f a
  App a b >>= f = App (a >>= f) (b >>= f)
  Lam b >>= f = Lam (b >>= traverse f)

lam :: Eq a => a -> Exp a -> Exp a
lam a = Lam . fmap (\a' -> if a == a' then Nothing else Just a')

whnf :: Exp a -> Exp a
whnf (Var a) = Var a
whnf (Lam a) = Lam a
whnf (App f x) = case whnf f of
  Lam b -> whnf $ b >>= maybe x Var
  f' -> App f' x

parse :: String -> Exp String
parse = fst . head . R.readP_to_S (space *> pExp <* R.eof)

space :: ReadP ()
space = void $ R.many $ R.satisfy isSpace

symbol :: ReadP String
symbol = R.munch1 isAlphaNum <* space

char :: Char -> ReadP ()
char c = void $ R.char c <* space

pTerm :: ReadP (Exp String)
pTerm = (Var <$> symbol) <|> (char '(' *> pExp <* char ')')

pExp :: ReadP (Exp String)
pExp = liftA2 lam (symbol <* char '.') pExp <|> (foldl1 App <$> R.many1 pTerm)
