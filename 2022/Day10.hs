splitList :: Eq a => a -> [a] -> [[a]]
splitList _ [] = []
splitList _ [x] = [[x]]
splitList s (x:xs)
    | s == x = []:next
    | otherwise = (x:(head next)):(tail next)
    where next = splitList s xs

noop :: [Int] -> [Int]
noop (x:xs) = x:x:xs

addx :: Int -> [Int] -> [Int]
addx i (x:xs) = (i+x):x:x:xs

parseCommand :: String -> [Int] -> [Int]
parseCommand xs
    | head parts == "noop" = noop
    | head parts == "addx" = addx (read (parts !! 1))
    where parts = splitList ' ' xs

get20s :: [Int] -> Int
get20s xs = sum [(xs !! (i - 1)) * i | i<-[1..length xs], i `mod` 40 == 20]

getScreenChar :: Bool -> Char
getScreenChar False = '.'
getScreenChar True = '#'

getScreenLine :: [Int] -> [Char]
getScreenLine xs = [getScreenChar (abs ((xs !! i) - i) <= 1) | i<-[0..length xs - 1]]

getScreenLines :: [Int] -> [Char]
getScreenLines xs = foldr1 (\a -> \b -> a++['\n']++b) [getScreenLine (take 40 . drop i $ xs) | i<-[0..length xs], i `mod` 40 == 0]

main :: IO ()
main = do
    contents <- readFile "day10.txt"
    let input = reverse . map parseCommand . lines $ contents
        list = reverse . foldr (\f -> \xs -> f xs) [1] $ input
    print (get20s list)
    putStrLn (getScreenLines list)
