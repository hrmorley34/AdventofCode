parseLine :: [String] -> (Int, Int)
parseLine ("forward":a:[]) = (read a, 0)
parseLine ("down":a:[]) = (0, read a)
parseLine ("up":a:[]) = (0, negate (read a))

readCmds :: FilePath -> IO [(Int, Int)]
readCmds f = do
    contents <- readFile f
    return (map (parseLine . words) (lines contents))

main :: IO ()
main = do
    moves <- readCmds "day02.txt"
    print (part1 moves)
    print (part2 moves)


part1 :: [(Int, Int)] -> Int
part1 xs = (uncurry (*)) (foldl (\(h, d) -> \(dh, dd) -> (h+dh, d+dd)) (0, 0) xs)

part2 :: [(Int, Int)] -> Int
part2 xs = (\(h, d, a) -> h*d) (foldl (\(h, d, a) -> \(dh, da) -> (h+dh, d+(dh*a), a+da)) (0, 0, 0) xs)
