desnafu :: Num a => String -> a
desnafu "=" = -2
desnafu "-" = -1
desnafu "0" = 0
desnafu "1" = 1
desnafu "2" = 2
desnafu (x:xs) = (desnafu [x]) * (5 ^ length xs) + (desnafu xs)

snafu_mod :: Integral a => a -> a
snafu_mod x
    | m > 2 = m - 5
    | otherwise = m
    where m = x `mod` 5

ensnafu :: (Eq a, Integral a) => a -> String
ensnafu (-2) = "="
ensnafu (-1) = "-"
ensnafu 0 = "0"
ensnafu 1 = "1"
ensnafu 2 = "2"
ensnafu i = ensnafu ((i-extra) `div` 5) ++ ensnafu extra
    where extra = snafu_mod i

main :: IO ()
main = do
    contents <- readFile "day25.txt"
    let output = ensnafu . sum . map desnafu . lines $ contents
    print (output)
