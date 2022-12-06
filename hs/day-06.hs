main = do
    text <- readFile "inputs/6.txt"
    print $ uniqueIndex 4 text
    print $ uniqueIndex 14 text

anyIdentical s = f [] s
    where 
        f a (s:sx) = s `elem` a || f (s:a) sx
        f a [] = False

uniqueIndex c s = c + idx c s
    where 
        idx c s = if anyIdentical $ take c s then 1 + idx c (tail s) else 0