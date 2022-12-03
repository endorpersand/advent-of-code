type Move = Int   -- 0, 1, 2 = R, P, S
type Winner = Int -- 0, 1, 2 = Draw, opp, you

main = do
    text <- readFile "inputs/2.txt"
    let games = lines text

    -- part A
    print $ sum $ fmap (calcGame1Score . game1Tuple) games

    -- part B
    print $ sum $ fmap (calcGame2Score . game2Tuple) games

-- part A
asMove :: Char -> Move
asMove 'A' = 0
asMove 'X' = 0
asMove 'B' = 1
asMove 'Y' = 1
asMove 'C' = 2
asMove 'Z' = 2
asMove _   = error "not a move"

game1Tuple [a, ' ', b] = (asMove a, asMove b)
game1Tuple _ = error "write it normally"

winner :: (Move, Move) -> Winner
winner (a, b) = (3 + a - b) `mod` 3

winnerScore :: Winner -> Int
winnerScore 0 = 3
winnerScore 1 = 0
winnerScore 2 = 6
winnerScore x = error $ "Invalid winner " ++ show x

moveScore :: Move -> Int
moveScore = (+1)

calcGame1Score (a, b) = winnerScore (winner (a, b)) + moveScore b

-- part B
asWinner :: Char -> Winner
asWinner 'X' = 1
asWinner 'Y' = 0
asWinner 'Z' = 2
asWinner _   = error "not a winner type"

move :: (Move, Winner) -> Move
move = winner

game2Tuple [a, ' ', b] = (asMove a, asWinner b)
game2Tuple _ = error "write it normally"

calcGame2Score (a, b) = winnerScore b + moveScore (move (a, b))