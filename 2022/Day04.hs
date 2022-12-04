splitOnChar :: Char -> [Char] -> [String]
splitOnChar _ [] = [""]
splitOnChar c (x:xs)
    | x == c = "":next
    | otherwise = (x:head next):tail next
    where next = splitOnChar c xs

parseLine :: String -> [[Int]]
parseLine line = map (map read . splitOnChar '-') (splitOnChar ',' line)

checkShape :: [[Int]] -> Bool
checkShape xs
    | length xs == 2 && length (xs !! 0) == 2 && length (xs !! 1) == 2 = True
    -- no other case

contains :: Ord a => [a] -> [a] -> Bool
contains paira pairb = (paira !! 0 <= pairb !! 0) && (paira !! 1 >= pairb !! 1)
bicontains :: Ord a => [a] -> [a] -> Bool
bicontains paira pairb = contains paira pairb || contains pairb paira

overlaps :: Ord a => [a] -> [a] -> Bool
overlaps paira pairb = (paira !! 0 <= pairb !! 1) && (paira !! 1 >= pairb !! 0)

match :: (b -> b -> Bool) -> [[b]] -> Int
match func = length . filter (uncurry func) . map (\t -> (t !! 0, t !! 1))

part1 :: [[[Int]]] -> Int
part1 = match bicontains

part2 :: [[[Int]]] -> Int
part2 = match overlaps

main :: IO ()
main = do
    contents <- readFile "day04.txt"
    let pairs = map parseLine . lines $ contents
    -- print (all checkShape pairs)
    print (part1 pairs)
    print (part2 pairs)
