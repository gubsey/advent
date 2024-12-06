import Data.Bifunctor (bimap)
import Data.Map.Strict qualified as M

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

splitByOnce x = (\(x:xs) -> (x,concat xs)) . splitBy x 

parseInput = bimap fa fb . splitByOnce (== "") . lines
  where
    fa :: [String] -> M.Map Int Int
    fa = M.fromList . map (bimap read read . splitByOnce (== '|'))
    fb :: [String] -> [[Int]]
    fb = map $ map read . splitBy (== ',')

main = do
  (orders, updates) <- parseInput <$> readFile "ex.txt"
  mapM_ print . M.assocs $ orders
  mapM_ print updates
  
