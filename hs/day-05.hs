main = do
    text <- readFile "inputs/5.txt"
    -- I didn't want to do parsing fr, so you get this.
    let stack = ["VQWMBNZC", "BCWRZH", "JRQF", "TMNFHWSZ", "PQNLWFG", "WPL", "JQCGRDBV", "WBNQZ", "JTGCFLH"]
    let moves = [ Move 2 1 7, Move 6 2 6, Move 10 7 6, Move 4 3 1, Move 5 6 4, Move 1 1 9, Move 4 6 9, Move 12 4 1, Move 5 1 4, Move 7 9 8, Move 11 8 1, Move 6 6 2, Move 2 5 2, Move 3 6 3, Move 4 9 4, Move 2 2 5, Move 1 6 4, Move 3 3 6, Move 1 8 4, Move 1 6 1, Move 28 1 4, Move 28 4 5, Move 1 9 1, Move 4 4 1, Move 2 6 2, Move 2 1 6, Move 7 4 2, Move 14 2 9, Move 1 4 1, Move 1 1 2, Move 18 5 6, Move 2 2 6, Move 1 9 7, Move 8 9 2, Move 15 6 5, Move 1 6 3, Move 3 2 5, Move 1 7 5, Move 2 1 3, Move 3 2 1, Move 1 6 4, Move 5 6 5, Move 2 2 9, Move 35 5 7, Move 4 9 3, Move 1 4 1, Move 5 1 7, Move 6 5 3, Move 1 9 4, Move 11 7 6, Move 2 9 2, Move 1 4 7, Move 14 7 4, Move 5 6 9, Move 2 2 4, Move 6 7 9, Move 2 9 5, Move 6 9 5, Move 8 4 9, Move 5 4 3, Move 3 5 7, Move 1 3 9, Move 5 3 4, Move 7 9 8, Move 2 7 4, Move 4 5 7, Move 1 5 3, Move 5 6 4, Move 8 4 8, Move 5 7 6, Move 1 4 7, Move 3 6 9, Move 2 6 5, Move 7 8 3, Move 2 5 9, Move 17 3 1, Move 3 1 3, Move 6 8 9, Move 4 4 7, Move 6 3 5, Move 2 8 5, Move 14 7 5, Move 2 4 5, Move 6 9 5, Move 1 7 9, Move 1 6 9, Move 8 1 9, Move 8 5 2, Move 2 1 3, Move 7 2 6, Move 2 3 4, Move 1 2 6, Move 3 1 6, Move 16 9 4, Move 2 9 8, Move 1 1 6, Move 2 9 4, Move 1 6 9, Move 1 6 1, Move 1 1 7, Move 1 6 9, Move 1 9 3, Move 1 3 8, Move 1 9 2, Move 1 2 7, Move 2 5 3, Move 7 5 8, Move 2 7 9, Move 1 6 7, Move 3 6 9, Move 10 8 7, Move 1 4 3, Move 3 3 1, Move 1 7 1, Move 19 4 6, Move 3 9 7, Move 1 9 2, Move 2 1 7, Move 1 9 1, Move 12 6 9, Move 2 7 1, Move 1 2 4, Move 11 6 3, Move 1 4 8, Move 1 6 8, Move 11 7 9, Move 2 8 9, Move 18 9 6, Move 5 3 7, Move 5 3 8, Move 11 5 6, Move 26 6 4, Move 1 6 5, Move 1 3 7, Move 3 8 3, Move 1 8 7, Move 3 3 6, Move 5 9 3, Move 1 4 9, Move 8 4 5, Move 2 7 8, Move 3 3 6, Move 3 4 6, Move 7 7 4, Move 1 9 1, Move 5 5 3, Move 2 9 7, Move 3 8 2, Move 7 3 7, Move 1 7 6, Move 3 5 6, Move 7 4 8, Move 10 4 5, Move 2 4 2, Move 3 7 5, Move 2 4 1, Move 6 8 5, Move 5 1 4, Move 5 4 2, Move 5 7 8, Move 10 2 8, Move 3 8 3, Move 2 5 3, Move 13 6 1, Move 19 5 3, Move 12 3 9, Move 4 8 2, Move 2 6 7, Move 5 8 7, Move 9 3 9, Move 1 5 9, Move 2 7 6, Move 3 2 3, Move 15 9 3, Move 13 3 5, Move 1 6 2, Move 5 5 8, Move 1 2 5, Move 1 7 6, Move 6 9 6, Move 6 6 8, Move 4 7 1, Move 2 3 6, Move 11 1 9, Move 1 2 3, Move 4 5 6, Move 1 1 6, Move 10 9 2, Move 8 2 3, Move 3 1 2, Move 8 3 1, Move 5 5 4, Move 1 9 8, Move 2 3 7, Move 2 4 5, Move 6 1 6, Move 9 8 1, Move 16 1 9, Move 2 7 3, Move 3 3 8, Move 6 9 6, Move 1 5 4, Move 1 3 8, Move 5 2 1, Move 5 1 9, Move 2 4 9, Move 4 8 6, Move 1 8 7, Move 4 8 5, Move 2 8 2, Move 17 9 5, Move 11 5 7, Move 1 2 5, Move 1 2 5, Move 1 9 1, Move 1 1 6, Move 5 7 6, Move 20 6 7, Move 4 6 4, Move 15 7 8, Move 2 3 7, Move 1 6 5, Move 10 8 4, Move 1 3 6, Move 4 6 4, Move 13 7 8, Move 1 7 5, Move 1 6 3, Move 1 6 3, Move 1 6 9, Move 9 4 1, Move 3 8 2, Move 14 5 6, Move 2 2 8, Move 1 3 9, Move 14 6 2, Move 1 3 9, Move 1 9 3, Move 15 2 1, Move 1 3 9, Move 4 4 9, Move 10 8 5, Move 1 9 5, Move 1 1 5, Move 4 8 7, Move 3 9 3, Move 1 8 5, Move 1 4 7, Move 2 8 7, Move 6 5 6, Move 4 1 2, Move 1 2 5, Move 2 2 8, Move 2 8 1, Move 3 7 2, Move 3 4 9, Move 18 1 8, Move 1 7 3, Move 3 9 6, Move 1 1 5, Move 5 6 4, Move 2 1 9, Move 8 4 5, Move 4 3 2, Move 16 5 4, Move 8 8 6, Move 2 2 6, Move 1 7 6, Move 7 8 1, Move 1 2 3, Move 2 8 3, Move 4 4 9, Move 4 1 2, Move 1 7 2, Move 1 5 4, Move 1 3 7, Move 3 4 5, Move 1 9 6, Move 9 2 5, Move 2 3 6, Move 3 5 8, Move 3 1 7, Move 4 5 8, Move 1 4 3, Move 5 9 5, Move 5 5 8, Move 1 3 4, Move 4 5 1, Move 2 5 4, Move 13 6 2, Move 12 2 9, Move 3 9 2, Move 4 1 6, Move 8 6 2, Move 1 4 9, Move 3 7 9, Move 2 9 8, Move 1 7 2, Move 9 9 5, Move 2 8 6, Move 4 2 3, Move 1 7 2, Move 1 6 4, Move 4 3 9, Move 9 5 8, Move 10 4 2, Move 1 4 7, Move 1 6 2, Move 1 6 7, Move 13 2 6, Move 1 2 5, Move 6 6 5, Move 7 5 8, Move 1 4 5, Move 27 8 5, Move 3 6 3, Move 2 8 6, Move 8 9 5, Move 1 7 9, Move 1 6 2, Move 4 5 9, Move 2 3 4, Move 9 2 5, Move 1 4 1, Move 1 4 2, Move 1 2 4, Move 1 3 7, Move 1 1 3, Move 1 3 9, Move 6 9 4, Move 1 7 5, Move 13 5 2, Move 1 9 5, Move 1 7 2, Move 5 2 7, Move 8 5 7, Move 6 4 2, Move 1 4 5, Move 3 2 4, Move 4 2 7, Move 2 4 3, Move 13 7 3, Move 5 2 3, Move 4 7 8, Move 11 3 8, Move 11 5 9, Move 4 6 9, Move 1 6 5, Move 1 4 2, Move 1 3 6, Move 3 2 6, Move 3 6 2, Move 1 6 1, Move 1 3 8, Move 3 3 6, Move 2 2 7, Move 4 3 9, Move 16 9 2, Move 1 7 8, Move 2 2 8, Move 9 2 3, Move 6 2 7, Move 1 6 3, Move 2 9 2, Move 1 9 7, Move 2 6 3, Move 4 3 9, Move 2 2 7, Move 1 2 5, Move 14 5 6, Move 14 6 3, Move 4 9 8, Move 5 8 4, Move 1 1 5, Move 4 8 1, Move 1 5 9, Move 8 7 2, Move 18 3 7, Move 1 1 5, Move 1 1 9, Move 1 4 5, Move 1 8 5, Move 8 2 9, Move 3 5 8, Move 7 7 1, Move 3 4 7, Move 1 3 6, Move 7 8 3, Move 2 9 3, Move 3 8 9, Move 9 1 7, Move 9 3 4, Move 2 3 4, Move 12 7 4, Move 1 3 8, Move 1 8 7, Move 8 4 7, Move 11 4 9, Move 5 4 8, Move 19 7 9, Move 1 6 2, Move 2 7 4, Move 2 8 3, Move 1 7 8, Move 1 3 2, Move 3 8 4, Move 1 8 9, Move 1 3 2, Move 36 9 1, Move 5 9 6, Move 5 4 2, Move 24 1 3, Move 5 6 7, Move 1 1 4, Move 14 3 4, Move 4 7 3, Move 1 8 5, Move 5 2 9, Move 1 1 6, Move 5 9 1, Move 3 2 3, Move 1 5 3, Move 11 4 2, Move 1 7 1, Move 6 1 9, Move 3 4 2, Move 1 6 7, Move 10 1 7, Move 3 2 1, Move 3 3 2, Move 2 1 7, Move 1 4 8, Move 13 3 2, Move 1 8 3, Move 2 7 5, Move 2 3 7, Move 2 5 2, Move 1 1 7, Move 28 2 6, Move 1 2 3, Move 2 8 2, Move 6 9 7, Move 1 3 8, Move 1 9 8, Move 3 6 2, Move 14 7 9, Move 3 2 1, Move 2 2 9, Move 2 1 9, Move 1 9 1, Move 7 6 9, Move 2 1 4, Move 2 4 6, Move 4 8 7, Move 1 7 6, Move 1 8 1, Move 1 3 6, Move 1 1 5, Move 14 9 8, Move 1 5 9, Move 5 7 3, Move 16 6 3, Move 2 7 4, Move 8 9 5, Move 6 6 1, Move 8 5 9, Move 2 7 4, Move 11 9 1, Move 4 4 1, Move 14 8 3, Move 2 1 7, Move 20 3 6, Move 5 3 1, Move 1 3 5, Move 2 7 4, Move 20 6 7, Move 18 7 6, Move 17 6 9, Move 1 5 3, Move 6 3 2, Move 3 3 1, Move 1 6 2, Move 2 7 8, Move 4 1 5, Move 2 4 9, Move 1 3 2, Move 1 8 6, Move 18 1 4, Move 1 2 7, Move 1 6 2, Move 3 4 3, Move 1 8 1, Move 4 1 6, Move 7 2 1, Move 1 5 7, Move 1 4 1, Move 2 6 3, Move 3 5 9, Move 9 9 8, Move 10 9 3, Move 9 3 5 ]
    let moves' = fmap (\ (Move ct i j) -> Move ct (i - 1) (j - 1)) moves

    let processedStack1 = foldl applyMove1 stack moves'
    -- part A
    print $ fmap head processedStack1

    -- part B
    let processedStack2 = foldl applyMove2 stack moves'
    print $ fmap head processedStack2

-- part A
type Stacks a = [[a]]
data Move = Move Int Int Int

popped (a:ax) = ax
popped [] = error "cannot pop"

stackPop i s = case splitAt i s of
    (sl, si:sr) -> (sl ++ (six : sr), head sih)
        where (sih, six) = splitAt 1 si
    (sl, []) -> error "cannot pop"
stackPush i v s = case splitAt i s of
    (sl, si:sr) -> sl ++ ((v:si) : sr)
    (sl, []) -> error "cannot push"

stackTransfer i j s = stackPush j it s'
    where (s', it) = stackPop i s

applyMove1 s (Move 0 i j) = s
applyMove1 s (Move ct i j) = applyMove1 (stackTransfer i j s) (Move (ct - 1) i j)

-- part B
stackRemove i n s = case splitAt i s of
    (sl, si:sr) -> (sl ++ (six : sr), sih)
        where (sih, six) = splitAt n si
    (sl, []) -> error "cannot pop"

stackAppend i v s = case splitAt i s of
    (sl, si:sr) -> sl ++ ((v ++ si) : sr)
    (sl, []) -> error "cannot push"

applyMove2 s (Move ct i j) = stackAppend j els s'
    where (s', els) = stackRemove i ct s