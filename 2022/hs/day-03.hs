import Data.Char
type Racksack = String

main = do
    text <- readFile "inputs/3.txt"

    -- part A
    let prios = do 
        racksack <- lines text
        let match = uncurry intersect (splitRacksack racksack)
        return $ priority (head match)
    print $ sum prios

    -- part B
    let racksacks = lines text
    let prios = do 
        triplet <- chunk 3 racksacks
        let (a:ax) = triplet
        return $ priority (head (foldr intersect a ax))
    print $ sum prios

-- part A
splitRacksack :: Racksack -> (Racksack, Racksack)
splitRacksack racksack = splitAt (length racksack `div` 2) racksack

intersect [] = const []
intersect a = filter (`elem` a)

priority c
    | isAsciiLower c = ord c - 0x60
    | isAsciiUpper c = ord c - 0x40 + 26
    | otherwise = error "you have no priority"

-- part B
chunk n [] = []
chunk n l = a : chunk n b
    where (a, b) = splitAt n l