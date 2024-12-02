import PuzzleInput

parseLine :: (Num a, Read a) => String -> [a]
parseLine = map read . words

diffs :: (Num a) => [a] -> [a]
diffs rs = zipWith (-) rs (tail rs)

checkReport :: (Num a, Ord a) => [a] -> Bool
checkReport rs = (all (< 0) d || all (> 0) d) && all ((1 <=) . abs) d && all ((<= 3) . abs) d
  where
    d = diffs rs

getReportPop :: (Num a, Ord a) => [a] -> [[a]]
getReportPop (r : rs) = rs : map (r :) (getReportPop rs)
getReportPop [] = []

checkReportPop :: (Num a, Ord a) => [a] -> Bool
checkReportPop rs = checkReport rs || any checkReport (getReportPop rs)

countTrue :: [Bool] -> Int
countTrue = length . filter id

main :: IO ()
main = do
  contents <- puzzleInputLines
  let reports = map parseLine contents
  print (countTrue . map checkReport $ reports)
  print (countTrue . map checkReportPop $ reports)
