import Control.Applicative (liftA2)
import Data.Functor
import Data.List
import Numeric (showHex)

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
    <$> reverse . transpose . zipWith drop [0 ..]
    <*> transpose . map reverse . zipWith take [0 ..]

droppingDiags = map ((\[a, b] -> [dr a, dl b]) . replicate 2) . tails
  where
    dr = transpose . zipWith drop [0 ..]
    dl = reverse . transpose . zipWith drop [0 ..] . map reverse

a <||> b = liftA2 (||) a b

makeLL n = take n . chunk n $ [10 ..]

splitHalf l = splitAt ((length l + 1) `div` 2) l

main = do
  ex <- lines <$> getContents
  print . length . filter (== "XMAS") . toNs 4 . concat . concatMap (\x -> [x, diagonals x]) . take 4 . iterate rotate $ ex
  print . length . concatMap (filter (all ((== "MAS") <||> (== "SAM"))) . transpose . map (toNs 3 . map (take 3))) . droppingDiags $ ex
