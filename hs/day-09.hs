main = do
    input <- readFile "inputs/9.txt"

    let shifts = do
        line <- lines input
        parseShift line
    print $ length $ allTails (replicate (1 + 1) (0, 0)) shifts
    print $ length $ allTails (replicate (1 + 9) (0, 0)) shifts

moveTail (hx, hy) (tx, ty)
    | abs dx <= 1 && abs dy <= 1 = (tx, ty)
    | otherwise = (tx + signum dx, ty + signum dy)
    where (dx, dy) = (hx - tx, hy - ty)

shift (dx, dy) ((lx, ly):lt) = m [] (lx + dx, ly + dy) lt
    where
        m done c [] = (reverse $ c:done, c)
        m done c (a:ax) = m (c:done) (moveTail c a) ax
shift _ [] = error "cannot shift empty rope"

parseShift (ds:' ':ns) = replicate (read ns) $ case ds of
    'L' -> (-1,  0)
    'R' -> ( 1,  0)
    'U' -> ( 0,  1)
    'D' -> ( 0, -1)
    _ -> error "not a shift"
parseShift _ = error "not a shift"

allTails = t []
    where
        -- tails, rope, shifts
        t ts _ [] = ts
        t ts rope (s:sx) = t ([rlast | rlast `notElem` ts] ++ ts) rope' sx
            where (rope', rlast) = shift s rope