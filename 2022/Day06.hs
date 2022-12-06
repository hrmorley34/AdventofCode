import Data.List

unique :: Eq a => [a] -> Bool
unique [] = True
unique [x] = True
unique (x:xs) = all (/= x) xs && unique xs

find_marker :: Eq a => Int -> [a] -> Int
find_marker n xs = head [i | i<-[n..(length xs - 1)], unique (take n . drop (i-n) $ xs)]

main :: IO ()
main = do
    putStr "> "
    contents <- getLine
    print (find_marker 4 contents)
    print (find_marker 14 contents)
