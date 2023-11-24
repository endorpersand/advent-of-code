import Control.Monad.Trans.State
import Data.List (stripPrefix, sort)
import Data.Maybe

main = do
    input <- readFile "inputs/11.txt"
    -- print $ lines input

    let business = (intoBusiness . makeMonkeys . lines) input

    let Business _ _ inspects = iter 20 (performRound wl1) business
    let a:b:c = (reverse . sort) inspects
    print $ a * b

    let Business _ _ inspects = iter 10000 (performRound $ wl2 business) business
    let a:b:c = (reverse . sort) inspects
    print $ a * b

monkeyState :: State [[Integer]] a -> State MonkeyBusiness a
monkeyState s = state $ \(Business a stack b) -> let (out, stack') = runState s stack in (out, Business a stack' b)

stackPop :: Int -> State [[a]] a
stackPop i = state pop
    where
        pop s = case splitAt i s of
            (sl, si:sr) -> case si of
                (sih:six) -> (sih, sl ++ (six : sr))
                [] -> error $ "cannot pop at " ++ show i
            (sl, []) -> error "cannot pop"

monkeyPop i = do
    Business monkeys stack inspect <- get

    let inspectLeft = take i inspect
    let (v:inspectRight) = drop i inspect
    let inspect' = inspectLeft ++ (v + 1):inspectRight
    put (Business monkeys stack inspect')

    monkeyState $ stackPop i

stackPush :: Int -> a -> State [[a]] ()
stackPush i v = state push
    where
        push s = case splitAt i s of
            (sl, si:sr) -> ((), sl ++ ((v:si) : sr))
            (sl, []) -> error "cannot push"

monkeyPush i = monkeyState . stackPush i

stackTransfer m i = do
    popped <- monkeyPop i
    Business monkeys _ _ <- get
    let Monkey w d (t, f) = monkeys !! i
    let outgoing = (m . w) popped
    let j = if outgoing `mod` d == 0 then t else f
    monkeyPush j outgoing

performRound m b @ (Business _ items _) = foldl (flip $ fullTransfer m) b [0..(length items - 1)]
wl1 = (`div` 3)
wl2 (Business monkeys _ _) = (`mod` (product . fmap (\(Monkey _ t _) -> t)) monkeys)

while :: (s -> Bool) -> State s () -> s -> s
while pred s init = if pred init then while pred s (execState s init) else init
until' pred = while (not . pred)

fullTransfer m i = until' (\(Business _ items _) -> null (items !! i)) (stackTransfer m i)

iter c f
    | c <= 0 = id
    | otherwise = f . iter (c - 1) f

data Monkey = Monkey {
    worry :: Integer -> Integer,
    test_divisor :: Integer,
    pass :: (Int, Int) -- true, false
}

data MonkeyBusiness = Business {
    monkeys :: [Monkey],
    items :: [[Integer]],
    inspects :: [Integer]
}

makeMonkeys :: [[Char]] -> [(Monkey, [Integer])]
makeMonkeys [] = []
makeMonkeys lines = parseMonkey m : case rest of
        "":next -> makeMonkeys next
        [] -> []
        _ -> error "unreachable"
    where
        splitMonkeyText = break null
        (m, rest) = splitMonkeyText lines

        stripPrefix' p = fromJust . stripPrefix p

        parseMonkey [_, items, op, test, true_cond, false_cond] = (
            Monkey {
                worry = case stripPrefix' "  Operation: new = old " op of
                    ('+':' ':val) -> (+ read val)
                    "* old" -> (\a -> a * a)
                    ('*':' ':val) -> (* read val)
                    _ -> error "unreachable",
                test_divisor = read $ stripPrefix' "  Test: divisible by " test,
                pass = (
                    read $ stripPrefix' "    If true: throw to monkey " true_cond,
                    read $ stripPrefix' "    If false: throw to monkey" false_cond
                )
            },
            read $ '[' : stripPrefix' "  Starting items: " items ++ "]"
            )
        parseMonkey _ = error "unreachable"

intoBusiness ab = Business a b (replicate (length b) 0)
    where (a, b) = unzip ab