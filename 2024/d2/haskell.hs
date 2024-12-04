import Data.List

main = do
  cat <- getContents
  print . length . filter isSafe . reports $ cat
  print . length . filter isSafe2 . reports $ cat

reports :: String -> [[Int]]
reports = map (map read . words) . lines

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails

allTheSame :: (Eq a) => [a] -> Bool
allTheSame [] = True
allTheSame (x : xs) = all (== x) xs

between :: (Ord a) => a -> a -> a -> Bool
between x z y = (x <= y) && (y <= z)

isSafe :: [Int] -> Bool
isSafe l =
  let wd = windows 2 l
   in let cond1 = allTheSame . map (\[a, b] -> compare a b) $ wd
       in let cond2 = all (\[a, b] -> between 1 3 . abs $ a - b) wd
           in cond1 && cond2

isSafe2 :: [Int] -> Bool
isSafe2 l = isSafe l || any isSafe (eclipse l)

eclipse :: [a] -> [[a]]
eclipse l = zipWith (++) (inits l) (tail . tails $ l)
