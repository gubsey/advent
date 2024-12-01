import Data.List

main = do
  cat <- getContents
  print . p1 $ cat
  print . p2 $ cat
  
p1 = sum . map abs . foldl1 (zipWith (-)) . map sort . transpose . map (map read . words) . lines
p2 = sum . f . transpose . map (map read . words) . lines where f [a,b] = map (\x -> x * length (filter (== x) b)) a
