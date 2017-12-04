import Data.List (nubBy, sort)
import Data.Function (on)

main = do
  file <- readFile "4.txt"
  let valid p = filter (((nubBy p) >>= (==))  . words) (lines file)
  print $ length $ valid (==)
  print $ length $ valid ((==) `on` sort)
