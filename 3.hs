data Side = N | S | E | W deriving Show

depth :: Integer -> Integer
depth x = fromIntegral $ ceiling ((sqrt (fromInteger x) -1) / 2)

bounds :: Integer -> (Integer, Integer)
bounds d = ((2 * d - 1) ^ 2 + 1, (2 * d + 1) ^ 2) 

corners :: Integer -> (Integer, Integer, Integer, Integer)
corners d = (br, br - 6 * d, br - 4 * d, br - 2 * d)
  where
    (_, br) = bounds d

midpoints :: Integer -> (Integer, Integer, Integer, Integer)
midpoints d = (br - 7 * d, br - 5 * d, br - 3 * d, br - 1 * d)
  where
    (_, br) = bounds d

side :: Integer -> (Side, Integer, Integer)
side x
  | x < tr    = (E, d, dist me)
  | x < tl    = (N, d, dist mn)
  | x < bl    = (W, d, dist mw)
  | x < br    = (S, d, dist ms)
  | otherwise = (E, d, dist ms) -- x == br
  where
    (br, tr, tl, bl) = corners d
    (me, mn, mw, ms) = midpoints d
    d = depth x
    dist mp = abs (x - mp)

steps :: Integer -> Integer
steps x = d + s
  where
    (_, d, s) = side x 

main = do
  print $ steps 361527
