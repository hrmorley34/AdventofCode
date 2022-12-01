readNums :: FilePath -> IO [Int]
readNums f = do
    contents <- readFile f
    return ((map read) (lines contents))

parseLines :: [String] -> [Int] -> [Int]
parseLines [] ns = ns
parseLines (c:xs) (n:ns)
    | c == "" = parseLines xs (0:n:ns)
    | otherwise = parseLines xs ((n + read c):ns)

bottom1 :: [Int] -> Int
bottom1 [n] = n
bottom1 (n:ns) = min n (bottom1 ns)

top1 :: [Int] -> Int
top1 [n] = n
top1 (n:ns) = max n (top1 ns)

top :: Int -> [Int] -> [Int]
top 1 ns = [top1 ns]
top i (n:ns)
    | (length ns) < i = n:ns
    | all (>= n) ts = ts
    | otherwise = n : filter (> bottom1 ts) ts
    where ts = top i ns

part1 :: [Int] -> Int
part1 xs = top1 xs

part2 :: [Int] -> Int
part2 xs = sum (top 3 xs)

main :: IO ()
main = do
    contents <- readFile "day01.txt"
    let totals = parseLines (lines contents) [0]
    print (part1 totals)
    print (part2 totals)
