main = do
    text <- readFile "inputs/4.txt"

    -- PART A
    print $ sum [1 | line <- lines text, let (r1, r2) = asRanges line, subranges r1 r2 || subranges r2 r1]
    -- PART B
    print $ sum [1 | line <- lines text, let (r1, r2) = asRanges line, overlaps r1 r2]

asRanges s = case break (== ',') s of
        (left, ',':right) -> (asRange left, asRange right)
        t -> error ("not a pair of ranges " ++ show t)

asRange s = case break (== '-') s of
        (left, '-':right) -> Range (read left) (read right)
        _ -> error "not a range"

data Range = Range Int Int

-- PART A
subranges (Range sups supe) (Range subs sube) = sups <= subs && sube <= supe

-- PART B
overlaps (Range ls le) (Range rs re) = max ls rs <= min le re