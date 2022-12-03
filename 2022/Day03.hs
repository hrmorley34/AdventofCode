import Data.Maybe
import Data.List
import qualified Data.Set as Set

priorityScoreList :: [Char]
priorityScoreList = ['_'] ++ ['a'..'z'] ++ ['A'..'Z']
priorityScore :: Char -> Int
priorityScore c = fromMaybe 0 (findIndex (==c) priorityScoreList)

setIntersect :: Ord a => [a] -> [a] -> [a]
setIntersect a b = Set.toList $ Set.intersection (Set.fromList a) (Set.fromList b)
setIntersectN :: Ord a => [[a]] -> [a]
setIntersectN xs = Set.toList $ foldr1 Set.intersection $ map Set.fromList xs

intersectSplitBackpack :: Ord a => [a] -> [a]
intersectSplitBackpack bp = 
   setIntersect (take middle bp) (drop middle bp)
    where middle = length bp `div` 2

single :: [a] -> a
single [x] = x
-- Not defined for any other case

part1 :: [[Char]] -> Int
part1 = sum . map (priorityScore . single . intersectSplitBackpack)

part2 :: [[[Char]]] -> Int
part2 = sum . map (priorityScore . single . setIntersectN)

main :: IO ()
main = do
    contents <- readFile "day03.txt"
    let bps = lines contents
    print (part1 bps)
    let triples = [[bps !! i | i<-[x*3..x*3 + 2]] | x<-[0..length bps `div` 3 - 1]]
    print (part2 triples)
