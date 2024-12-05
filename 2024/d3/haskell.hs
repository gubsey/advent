{-# LANGUAGE DeriveFunctor, LambdaCase, TupleSections #-}

import Control.Monad
import Control.Applicative (liftA2)
import Data.List
import Data.Maybe
import Text.Read (readMaybe)
import Data.Char
import Data.Functor

newtype Parser a = Parser
  { runParser :: String -> (String, Maybe a)
  } deriving (Functor)
  
instance Applicative Parser where
  pure c = Parser (, Just c)
  pf <*> pa = Parser $ \s -> case runParser pf s of
    (s', Just f) -> fmap f <$> runParser pa s'
    (s', Nothing) -> (s', Nothing)

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    pa >>= f = Parser $ \s -> case runParser pa s of
        (s', Just a) -> runParser (f a) s'
        (s', Nothing) -> (s', Nothing)

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = try $ do
  c <- anyP
  if predicate c
    then pure c
    else nullParser

nullParser :: Parser a
nullParser = Parser (, Nothing)

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser $ \s -> case runParser p1 s of
  (s', Nothing)
    | s' == s   -> runParser p2 s
    | otherwise -> (s', Nothing)
  success -> success

many :: Parser a -> Parser [a]
many  p = many' p <|> pure [] where
  many' p = liftA2 (:) p $ many p

try p = Parser $ \s -> case runParser p s of
  (_s', Nothing) -> (s, Nothing)
  success -> success

char c = satisfy (==c)
space = satisfy isSpace

string :: String -> Parser String
string = traverse char

anyP :: Parser Char
anyP = Parser $ \case
  (x : xs) -> (xs, Just x)
  [] -> ("", Nothing)

choice :: [Parser a] -> Parser a
choice = foldr (<|>) nullParser

number :: Parser Int
number = read <$> many (satisfy isDigit)
spaces = many $ satisfy isSpace
symbol s = string s <* spaces


run p s = snd $ runParser p s

parseDo = string "do()"
parseDont = string "don't()"
parseMul = do
  _ <- string "mul("
  a <- number
  _ <- char ','
  b <- number
  _ <- char ')'
  pure $ a * b
parse = choice 
  [ Do <$ parseDo
  , Dont <$ parseDont
  , Mul <$> parseMul
  ]



data Parsed = Mul Int | Do | Dont
instance Show Parsed where
  show Do = "Do"
  show Dont = "Don't"
  show (Mul a) = show a

main = do
  cat <- getContents
  print . sum .mapMaybe (run parseMul) . tails $ cat
  print . mapMaybe (run parse) . tails $ cat
