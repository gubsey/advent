{-# LANGUAGE LambdaCase #-}

import Data.Bifunctor (bimap)
import Data.Function
import Data.List
import Data.Map.Strict qualified as M
import Data.Maybe

-- stolen from Data.List.lines and modified to be more generic
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy predicate s =
  cons
    ( case break predicate s of
        (l, s') ->
          ( l,
            case s' of
              [] -> []
              _ : s'' -> splitBy predicate s''
          )
    )
  where
    cons ~(h, t) = h : t

splitByOnce x = (\(x : xs) -> (x, concat xs)) . splitBy x

readInt = read :: String -> Int

mid t = m t t
  where
    m (x : _) [_] = x
    m (x : y : _) [_, _] = x
    m (_ : t) (_ : _ : u) = m t u

parseInput = bimap fa fb . splitByOnce (== "") . lines
  where
    fa = map (bimap readInt readInt . splitByOnce (== '|'))
    fb = map $ map readInt . splitBy (== ',')

valid orders x = x & subsequences & mapMaybe (\case [a, b] -> Just (b, a); _ -> Nothing) & intersect orders & (== [])

main = do
  (orders, updates) <- parseInput <$> getContents
  updates & filter (valid orders) & map mid & sum & print
