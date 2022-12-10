import Control.Monad.Trans.State

main = do
    input <- readFile "inputs/test/10a.txt"

    let insts = parseLine <$> lines input
    let state = put (Nothing, insts, 1) :: State InstState ()

    let st = do
        progress
        progress
        progress
        progress
    print $ while (not . isEmpty) progress (Nothing, insts, 1)
    error "todo"

data Inst = Noop | Addx Int
    deriving (Show, Eq)
type InstState = (Maybe Inst, [Inst], Int)

applyInst Noop = id
applyInst (Addx i) = (+i)

progress :: State InstState Int
progress = state p
    where
        p (Just inst, rest, i) = (i', (Nothing, rest, i'))
            where i' = applyInst inst i
        p (Nothing, Noop:rest, i) = (i, (Nothing, rest, i))
        p (Nothing, addx:rest, i) = (i, (Just addx, rest, i))
        p empty @ (_, _, i) = (i, empty)

isEmpty (Nothing, [], _) = True
isEmpty _ = False

parseLine "noop" = Noop
parseLine ('a':'d':'d':'x':' ':rest) = Addx $ read rest
parseLine _ = error "not a line"

while :: (s -> Bool) -> State s b -> s -> [b]
while pred s init = evalState s init : (if pred init then while pred s (execState s init) else [])