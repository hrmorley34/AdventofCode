pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

increased :: Ord a => a -> a -> Bool
increased a b = a < b

countIncreases :: (Num a, Ord b) => [b] -> a
countIncreases xs = sum (map (\b -> if b then 1 else 0) (map (uncurry increased) (pairs xs)))

part1 :: (Num a, Ord b) => [b] -> a
part1 xs = countIncreases xs

slidingSum :: Num b => [b] -> [b]
slidingSum xs = map (uncurry (+)) (
    zip 
        (map (uncurry (+)) (pairs xs))
        (tail (tail xs)))

part2 :: (Ord b, Num a, Num b) => [b] -> a
part2 xs = countIncreases (slidingSum xs)

readNums :: FilePath -> IO [Int]
readNums f = do
    contents <- readFile f
    return ((map read) (words contents))

main :: IO ()
main = do
    nums <- readNums "day01.txt"
    print (part1 nums)
    print (part2 nums)
