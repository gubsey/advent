import Data.List

main = do
  getContents >>= print . reports
reports :: String -> [[Int]]
reports = map (map read . words) . lines
