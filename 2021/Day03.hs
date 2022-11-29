readNums :: FilePath -> IO [[Bool]]
readNums f = do
    contents <- readFile f
    return (map boolifyNums (lines contents))

boolifyNums :: [Char] -> [Bool]
boolifyNums = map (== '1')

stringifyNums :: [Bool] -> [Char]
stringifyNums = map (\b -> if b then '1' else '0')

intifyNum :: Bool -> Int
intifyNum d = if d then 1 else 0

intifyNums :: [Bool] -> Int
intifyNums [d] = intifyNum d
intifyNums ns = (intifyNums (take (length ns - 1) ns)) * 2 + (intifyNum (last ns))

main :: IO ()
main = do
    nums <- readNums "day03.txt"
    print (part1 nums)
    print (part2 nums)


countBools :: [Bool] -> Int
countBools = sum . map intifyNum

getModal :: (Int -> Int -> Bool) -> [Bool] -> Bool
getModal comp xs = ((countBools xs * 2) `comp` (length xs))

findModes :: (Int -> Int -> Bool) -> [[Bool]] -> [Bool]
findModes comp xs = map (\i -> getModal comp (map (!! i) xs)) [0.. length (xs!!0) - 1]

part1 :: [[Bool]] -> Int
part1 xs = intifyNums (findModes (>=) xs) * intifyNums (findModes (<) xs)

getModalHead :: (Int -> Int -> Bool) -> [[Bool]] -> Bool
getModalHead comp xs = getModal comp (map head xs)

filterModes :: (Int -> Int -> Bool) -> [[Bool]] -> [Bool]
filterModes _ [] = []
filterModes _ [b] = b
filterModes comp xs = (\x ->
    x : (filterModes comp . map tail . filter (((x) ==) . head)) xs)
    (getModalHead comp xs)

part2 :: [[Bool]] -> Int
part2 xs = intifyNums (filterModes (>=) xs) * intifyNums (filterModes (<) xs)
