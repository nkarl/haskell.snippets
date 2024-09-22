{-# LANGUAGE MonoLocalBinds #-}

module Library.Monads.Parser where

import Control.Applicative (Alternative, (<|>))

import Data.Foldable ()
import Data.Kind (Type)
import Data.List (uncons)
import GHC.Unicode (isAlpha, isDigit)
import Text.Printf (IsChar (fromChar))
import Prelude

type State a = (String, a)

type Action e a = (Failing e) => String -> Either e (State a)

newtype ParseM e a = ParseM (Action e a)

data ErrorMsg
  = EOF
  | InvalidChar String
  deriving (Show)

class Failing (e :: Type) where
  eof :: e
  invalidSymbol :: String -> e

instance Failing ErrorMsg where
  eof = EOF
  invalidSymbol = InvalidChar

instance Functor (ParseM e) where
  fmap f (ParseM mx) = ParseM $ ((f <$>) <$>) . mx

instance Applicative (ParseM e) where
  (<*>) (ParseM mf) (ParseM mx) =
    ParseM $ \s -> do
      (s1, f) <- mf s
      (s2, x) <- mx s1
      pure (s2, f x)
  pure x = ParseM $ \s -> Right (s, x)

instance Monad (ParseM e) where
  (>>=) (ParseM mx) f =
    ParseM $ \s -> do
      (s', x) <- mx s
      ParseM g <- pure $ f x
      g s'

char :: forall e. ParseM e Char
char = ParseM $ \s -> case uncons s of
  Nothing -> Left eof
  Just (c, rest) -> Right (rest, c)

type Pair = (Char, Char)

twoChars :: forall e. ParseM e Pair
twoChars =
  -- do
  --    c1 <- char
  --    c2 <- char
  --    pure (c1, c2)
  (,) <$> char <*> char

-- threeChars :: forall e. ParseM String e (Char, (Char, Char))
-- threeChars = (,) <$> char <*> twoChars

threeChars :: forall e. ParseM e String
threeChars =
  -- do
  --    c1 <- char
  --    c2 <- char
  --    c3 <- char
  --    pure [c1, c2, c3]
  (\c1 c2 c3 -> [c1, c2, c3]) <$> char <*> char <*> char

unwrap :: forall e a. ParseM e a -> Action e a
unwrap (ParseM f) = f

parse :: ParseM ErrorMsg a -> Action ErrorMsg a
parse = unwrap

failedParsing :: forall e a. (Failing e) => e -> ParseM e a
failedParsing e = ParseM $ \_ -> Left e

satisfy :: forall e. (Failing e) => (Char -> Bool) -> String -> ParseM e Char
satisfy predicate reason =
  char >>= \c ->
    if predicate c
      then pure c
      else failedParsing $ invalidSymbol reason -- Left $ invalidSymbol expected

digit :: forall e. (Failing e) => ParseM e Char
digit = satisfy (isDigit . fromChar) "is not a digit."

letter :: forall e. (Failing e) => ParseM e Char
letter = satisfy (isAlpha . fromChar) "is not a letter."

-- alphaNum :: forall e. (Failing e) => ParseM e Char
-- alphaNum = ParseM $ \s -> case (parse letter s :: Either ErrorMsg (State Char)) of
--    Right x -> Right x
--    Left _ -> case unwrap digit s of
--        Left err -> Left err
--        Right y -> Right y
instance Alternative (ParseM e) where
  (<|>) p1 p2 =
    ParseM $ \s -> case unwrap p1 s of
      Left _ -> unwrap p2 s
      Right x -> Right x

alphaNum :: forall e. (Failing e) => ParseM e Char
alphaNum = letter <|> digit <|> failedParsing (invalidSymbol "is not an alphaNumeral.")

-- count :: (Traversable f) => (Foldable f) => Int -> ParseM e a -> ParseM e (f a)
-- count n p
--    | n <= 0 = pure mempty
--    | otherwise = sequence (replicate n p)

-- count' :: forall e. Int -> ParseM e Char -> ParseM e String
-- count' n p = ([]) <$> count n p

test :: IO ()
test = do
  let
    s = "ABC"
  print (parse char s :: Either ErrorMsg (State Char))
  print (parse twoChars s :: Either ErrorMsg (State (Char, Char)))
  print (parse threeChars s :: Either ErrorMsg (State String))

-- print $ parse (count' 3 digit) "1234567"
-- print $ parse (count' 3 digit) "abc1234"
-- print $ parse (count' 4 letter) "Freddy"
-- print $ parse (count' 10 alphaNum) "a1b2c3d4e5"
-- print $ parse (count' 10 alphaNum) "######"
