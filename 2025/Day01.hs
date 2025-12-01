import PuzzleInput

parseValue :: (Integral a, Read a) => String -> a
parseValue ('L' : ns) = -(read ns)
parseValue ('R' : ns) = read ns

accumTotal :: (Integral a) => a -> (a, a) -> (a, a)
accumTotal n (c, v)
  | xm == 0 = (c + 1, xm)
  | otherwise = (c, xm)
  where
    x = v + n
    xm = mod x 100

accumTotal2 :: (Integral a) => a -> (a, a) -> (a, a)
accumTotal2 n (c, v)
  | xm == 0 || (xm /= x && v /= 0) = (cx + 1, xm)
  | otherwise = (cx, xm)
  where
    cx = c + div (abs n) 100
    x = v + mod (abs n) 100 * signum n
    xm = mod x 100

main :: IO ()
main = do
  contents <- puzzleInputLines
  let changes = map parseValue contents
  print (fst . foldr accumTotal (0, 50) . reverse $ changes)
  print (fst . foldr accumTotal2 (0, 50) . reverse $ changes)
