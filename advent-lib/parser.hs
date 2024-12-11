{-# LANGUAGE LambdaCase #-}
import Data.Text.Internal.Read (IParser(runP))

newtype Parser a = Parser
  { runParser :: String -> (String, Maybe a)
  }
  deriving (Functor)

instance Applicative Parser where
    pure a = Parser (, Just a)
    pf <*> pa = Parser $ \s -> case runParser pf s of 
        (xs, Just f) -> fmap f <$> runParser pa xs 
        (xs, Nothing) -> (s, Nothing)

instance Monad Parser where
    pa >>= f = Parser $ \s -> case runParser pa s of 
        (xs, Just x) -> runParser (f x) xs
        (xs, Nothing) -> (s, Nothing) 

anyChar = Parser $ \case
    (x:xs) -> (xs, Just x)
    _ -> ([], Nothing)
