import Control.Applicative (liftA2)
import Data.Functor
import Data.List

printLists :: (Show a) => [[a]] -> IO ()
printLists = mapM_ print

rotate = transpose . reverse

counterRotate = reverse . transpose

toNs = concatMap . windows

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails

chunk _ [] = []
chunk n l = take n l : (chunk n . drop n $ l)

diagonals =
  (++)
    <$> transpose . zipWith drop [0 ..]
    <*> transpose . map reverse . zipWith take [0 ..]

a <||> b = liftA2 (||) a b

main = do
  ex <- lines <$> getContents
  print . length . filter (== "XMAS") . toNs 4 . concat . concatMap (\x -> [x, diagonals x]) . take 4 . iterate rotate $ ex
  printLists . filter (all ((== "MAS") <||> (== "SAM"))) . transpose . map (toNs 3 . diagonals) . take 2 . iterate rotate $ ex