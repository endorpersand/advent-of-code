import Data.List (sort)
main = do
    text <- readFile "inputs/1.txt"
    let partitions = sumSequences $ lines text
    print $ maximum partitions
    let sortedPartitions = sort partitions
    print $ sum $ take 3 $ reverse $ sort partitions

sumSequences :: [String] -> [Int]
sumSequences [] = []
sumSequences s = 
    sum (fmap read a) : sumSequences (dropWhile (== []) b)
    where (a, b) = break (== []) s
