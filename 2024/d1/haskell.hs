import Data.List

main = do
  cat <- getContents
  print . p1 $ cat
  print . p2 $ cat
  
p1 = sum . map abs . foldl1 (zipWith (-)) . map sort . transpose . map (map read . words) . lines
p2 s = 
  let [a, b] = transpose . map (map read . words) . lines $ s in
  let f x = x * length (filter (== x) b) in
  sum $ map f a
