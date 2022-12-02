readNums :: FilePath -> IO [String]
readNums f = do
    contents <- readFile f
    return (lines contents)

parseChar :: Num a => Char -> a
parseChar c
    | c=='A' || c=='X' = 1
    | c=='B' || c=='Y' = 2
    | c=='C' || c=='Z' = 3

parseLine :: Num a => String -> (a, a)
parseLine line = (parseChar (line !! 0), parseChar (line !! 2))

wrap3 :: Integral a => a -> a
wrap3 x = ((x - 1) `mod` 3) + 1

scoreMoves :: Integral a => (a, a) -> a
scoreMoves (m1, m2)
    | m1 == m2 = 3 + m2
    | wrap3 m1 == wrap3 (m2 - 1) = 6 + m2
    | wrap3 (m1 - 1) == wrap3 m2 = m2

part1 :: Integral a => [(a, a)] -> a
part1 = sum . map scoreMoves

scoreStates :: Integral a => (a, a) -> a
scoreStates (m1, w)
    | w == 1 = m2
    | w == 2 = 3 + m2
    | w == 3 = 6 + m2
    where m2 = wrap3 (m1 + (w-2))

part2 :: Integral a => [(a, a)] -> a
part2 = sum . map scoreStates

main :: IO ()
main = do
    contents <- readFile "day02.txt"
    let totals = map parseLine (lines contents)
    print (part1 totals)
    print (part2 totals)
