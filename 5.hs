import Control.Monad
import Control.Monad.Fix
import Data.Array.ST
import Data.Array.Base
import Data.Array.Unboxed
import Data.STRef

steps :: [Int] -> (Int -> Int) -> Int
steps xs rule = (! 0) $ runSTUArray $ do
   let bounds = (0, length xs - 1)
   instr <- listUArrayST bounds xs
   cntR  <- newSTRef 0
   pcR   <- newSTRef 0
   let f continue = do
        pc <- readSTRef pcR
        if inRange bounds pc 
        then do
           modifySTRef' cntR (+ 1)
           jump <- readArray instr pc
           modifySTRef' pcR (+ jump)
           writeArray instr pc (rule jump)
           continue
        else readSTRef cntR >>= writeArray instr 0
   fix f
   return instr

main = do
  file <- readFile "5.txt"
  let xs = map read . lines $ file
  print $ steps xs (+1)
  print $ steps xs (\x -> if x >= 3 then x - 1 else x + 1)
