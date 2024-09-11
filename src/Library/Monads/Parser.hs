{-# LANGUAGE MonoLocalBinds #-}
module Library.Monads.Parser where

-- import Control.Applicative (Alternative, empty, (<|>))

import Data.Foldable ()
import Data.Kind (Type)
import Data.List (uncons)
import GHC.Base (Alternative, (<|>))
import GHC.Unicode (isAlpha, isDigit)
import Text.Printf (IsChar (fromChar))
import Prelude

type State a = (String, a)

type Proc e a = (Fail e) => String -> Either e (State a)

newtype Parse e a = Parse (Proc e a)

data ErrorMsg
  = EOF
  | InvalidChar String
  deriving (Show)

class Fail (e :: Type) where
  eof :: e
  invalidChar :: String -> e

instance Fail ErrorMsg where
  eof = EOF
  invalidChar = InvalidChar

instance Functor (Parse e) where
  fmap f (Parse mx) = Parse (fmap (fmap f) . mx)

instance Applicative (Parse e) where
  (<*>) (Parse mf) (Parse mx) =
    Parse $ \s -> do
      (s1, f) <- mf s
      (s2, x) <- mx s1
      pure (s2, f x)
  pure x = Parse $ \s -> Right (s, x)

instance Monad (Parse e) where
  (>>=) (Parse mx) f =
    Parse $ \s -> do
      (s', x) <- mx s
      Parse g <- pure $ f x
      g s'

char :: forall e. Parse e Char
char = Parse $ \s -> case uncons s of
  Nothing -> Left eof
  Just (c, rest) -> Right (rest, c)

twoChars :: forall e. Parse e (Char, Char)
twoChars =
  -- (,) <$> char <*> char
  do
    c1 <- char
    c2 <- char
    pure (c1, c2)

-- threeChars :: forall e. Parse String e (Char, (Char, Char))
-- threeChars = (,) <$> char <*> twoChars

threeChars :: forall e. Parse e String
threeChars =
  -- (\c1 c2 c3 -> [c1, c2, c3]) <$> char <*> char <*> char
  do
    c1 <- char
    c2 <- char
    c3 <- char
    pure [c1, c2, c3]

unwrap :: forall e a. (Fail e) => Parse e a -> Proc e a
unwrap (Parse f) = f

parse :: Parse ErrorMsg a -> Proc ErrorMsg a
parse = unwrap

fail' :: forall e a. (Fail e) => e -> Parse e a
fail' e = Parse $ \_ -> Left e

satisfy :: forall e. (Fail e) => String -> (Char -> Bool) -> Parse e Char
satisfy expected predicate =
  char >>= \c ->
    if predicate c then pure c else fail' $ invalidChar expected -- Left $ invalidChar expected

digit :: forall e. (Fail e) => Parse e Char
digit = satisfy "digit" (isDigit . fromChar)

letter :: forall e. (Fail e) => Parse e Char
letter = satisfy "letter" (isAlpha . fromChar)

-- alphaNum :: forall e. (Fail e) => Parse e Char
-- alphaNum = Parse $ \s -> case (parse letter s :: Either ErrorMsg (State Char)) of
-- Right x -> Right x
-- Left _ -> case unwrap digit s of
-- Left err -> Left err
-- Right y -> Right y
instance Alternative (Parse e) where
  (<|>) p1 p2 =
    Parse $ \s -> case unwrap p1 s of
      Left _ -> unwrap p2 s
      Right x -> Right x

alphaNum :: forall e. (Fail e) => Parse e Char
alphaNum = letter <|> digit <|> fail' (invalidChar "alphaNum")

--count :: (Traversable f) => (Foldable f) => Int -> Parse e a -> Parse e (f a)
--count n p
  -- | n <= 0 = pure mempty
  -- | otherwise = sequence (replicate n p)

--count' :: forall e. Int -> Parse e Char -> Parse e String
--count' n p = ([]) <$> count n p

test :: IO ()
test = do
  let
    s = "ABC"
  print (parse char s :: Either ErrorMsg (State Char))
  print (parse twoChars s :: Either ErrorMsg (State (Char, Char)))
  print (parse threeChars s :: Either ErrorMsg (State String))
  --print $ parse (count' 3 digit) "1234567"
  --print $ parse (count' 3 digit) "abc1234"
  --print $ parse (count' 4 letter) "Freddy"
  --print $ parse (count' 10 alphaNum) "a1b2c3d4e5"
  --print $ parse (count' 10 alphaNum) "######"
