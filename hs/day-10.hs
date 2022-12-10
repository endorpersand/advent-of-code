import Control.Monad.Trans.State

main = do
    input <- readFile "inputs/10.txt"

    let insts = parseLine <$> lines input
    let state = put (Nothing, insts, 1) :: State InstState ()

    let signals = while (not . isEmpty) progress (Nothing, insts, 1)

    -- part A
    print $ sum $ dedup $ fmap (\i -> (signals !! (i - 1)) * i) [20, 60 .. 220]

    -- part B
    let cray = (\ (p, sig) -> sig - 1 <= p && p <= sig + 1) <$> zip (cycle [1..40]) signals
    let display1 = do
        (p, sig) <- zip (cycle [0..39]) signals
        let on = sig - 1 <= p && p <= sig + 1
        return $ if on then '#' else ' '

    foldl1 (>>) $ putStrLn <$> chunk 40 display1

data Inst = Noop | Addx Int
    deriving (Show, Eq)
type InstState = (Maybe Inst, [Inst], Int)

applyInst Noop = id
applyInst (Addx i) = (+i)

progress :: State InstState Int
progress = state p
    where
        p (Just inst, rest, i) = (i, (Nothing, rest, applyInst inst i))
        p (Nothing, Noop:rest, i) = (i, (Nothing, rest, i))
        p (Nothing, addx:rest, i) = (i, (Just addx, rest, i))
        p empty @ (_, _, i) = (i, empty)

isEmpty (Nothing, [], _) = True
isEmpty _ = False

parseLine "noop" = Noop
parseLine ('a':'d':'d':'x':' ':rest) = Addx $ read rest
parseLine _ = error "not a line"

while :: (s -> Bool) -> State s b -> s -> [b]
while pred s init = if pred init then evalState s init : while pred s (execState s init) else []

dedup a = d [] a
    where
        d buf (a:ax) = d ([a | a `notElem` buf] ++ buf) ax
        d buf [] = buf

chunk n [] = []
chunk n l = left : chunk n right
    where (left, right) = splitAt n l
