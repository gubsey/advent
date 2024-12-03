{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.Cont (when)
import Data.List
import Data.Maybe
import Text.Read (readMaybe)

newtype Parser a = Parser
  { runParser :: String -> (String, Maybe a)
  } deriving (Functor)
  
instance Applicative Parser where
  pure c = Parser $ \s -> (s, Just c)
  pf <*> pa = Parser $ \s -> case runParser pf s of
    (s', Just f) -> fmap f <$> runParser pa s'
    (s', Nothing) -> (s', Nothing)

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    pa >>= f = Parser $ \s -> case runParser pa s of
        (s', Just a) -> runParser (f a) s'
        (s', Nothing) -> (s', Nothing)

digit :: Parser Int
digit = Parser $ \case
    [] -> ("", Nothing)
    (x:xs) -> (xs, readMaybe [x])

char :: Char -> Parser ()
char c = Parser $ \case
    (x:xs) -> (xs, if x == c then Just () else Nothing)
    [] -> ("", Nothing)

str :: String -> Parser ()
str = foldr1 (>>) . map char

anyP :: Parser Char
anyP = Parser $ \case
  (x : xs) -> (xs, Just x)
  [] -> ("", Nothing)

eofP :: Parser ()
eofP = Parser $ \case
  [] -> ("", Just ())
  x -> (x, Nothing)

main = do
  cat <- getContents
  mapM_ print $ tokenizer tokens cat

tokens = ["mul(", "don't", "do"] ++ [show x | x <- [0 .. 9]] ++ [[x] | x <- "(),"]

tokenizer tokens = filter (\x -> any (`isPrefixOf` x) validStarts) . tails

validStarts = ["mul(", "don't()", "do()"]
