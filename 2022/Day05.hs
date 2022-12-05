splitList :: Eq a => a -> [a] -> [[a]]
splitList _ [] = []
splitList _ [x] = [[x]]
splitList s (x:xs)
    | s == x = []:next
    | otherwise = (x:(head next)):(tail next)
    where next = splitList s xs

parseStateLength :: [a] -> Int
parseStateLength line = length [line !! x | x<-[1..length line], x `mod` 4 == 1]

parseStateLine :: Int -> [a] -> [a]
parseStateLine size line = [line !! x | x<-[1..size * 4], x `mod` 4 == 1]

parseState :: Int -> [[Char]] -> [[Char]]
parseState size [] = ["" | _<-[1..size]]
parseState size (x:xs) = map (\(a,b) -> if a == ' ' then b else a:b) (zip (parseStateLine size x) (parseState size xs))

parseMove :: String -> Bool -> [[Char]] -> [[Char]]
parseMove line c9001 state
    | dest < src =
        (take (dest) state)
        ++ [taken ++ (state !! dest)]
        ++ (drop (dest+1) (take (src) state))
        ++ [drop count (state !! src)]
        ++ (drop (src+1) state)
    | src < dest =
        (take (src) state)
        ++ [drop count (state !! src)]
        ++ (drop (src+1) (take (dest) state))
        ++ [taken ++ (state !! dest)]
        ++ (drop (dest+1) state)
    where
        parts = splitList ' ' line
        count = read (parts !! 1)
        src = read (parts !! 3) - 1
        dest = read (parts !! 5) - 1
        taken
            | c9001 = take count (state !! src)
            | otherwise = reverse (take count (state !! src))

tops :: [[a]] -> [a]
tops [] = []
tops (x:xs) = (head x):(tops xs)

main :: IO ()
main = do
    contents <- readFile "day05.txt"
    let input = splitList "" . lines $ contents
        stateWidth = parseStateLength (last (input !! 0)) -- count using number row
        state = parseState stateWidth (init (input !! 0)) -- init trims off number row
        moves = map parseMove (input !! 1)
    print (tops (foldl (\s -> \f -> f False s) state moves))
    print (tops (foldl (\s -> \f -> f True s) state moves))
