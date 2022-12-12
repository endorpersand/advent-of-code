{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Data.Char
import Control.Monad
import Control.Monad.Trans.State
import Data.Map hiding (filter, foldr, null)
import Data.List (sort)
main = do
    input <- readFile "inputs/12.txt"
    let field = grid input
    let sPos = findPosEq field 0
    let ePos = findPosEq field 27

    let ds = DS field [(0, sPos)] empty
    -- print ePos
    -- let dijk50 = foldl1 (>>) (replicate 50 dijkstra)
    -- print $ execState dijk50 ds
    let DS _ _ traversed = dijkstraUntil ePos ds
    print $ traversed ! ePos

    let ds @ (DS _ frontier traversed) = dijkstraAll (DS (invertField field) [(0, ePos)] empty)
    
    let minStep = minimum $ do
        (y, row) <- zip [0..] field
        (x, e) <- zip [0..] row
        let pos = (x, y)
        guard (pos `member` traversed)
        guard (getHeight field pos <= 1)
        return (traversed ! pos)

    print minStep

mapping 'S' = 0
mapping 'E' = 27
mapping a = ord a - 96

grid :: String -> Field
grid m = fmap mapping <$> lines m

invertField :: (Functor f1, Functor f2, Num b) => f1 (f2 b) -> f1 (f2 b)
invertField = fmap (fmap (27 -))

findPosEq m v = head $ do
    (y, row) <- zip [0..] m
    (x, e) <- zip [0..] row
    guard (e == v)
    return (x, y)

type Field = [[Int]]
type Coord = (Int, Int)

data DijkstraState = DS {
    field :: Field,
    frontier :: [(Int, Coord)],
    traversed :: Map Coord Int
} deriving (Show, Eq)

getHeight f (x, y) = (f !! y) !! x

neighbors (a, b) = [(a + 1, b), (a - 1, b), (a, b + 1), (a, b - 1)]

boundTo field = filter (\ (a, b) -> 0 <= a && a < length (head field) && 0 <= b && b < length field)
restrictHeight field start = filter (\nb -> height nb <= height start + 1)
    where height = getHeight field

dijkstra :: State DijkstraState ()
dijkstra = do
    DS field frontier traversed <- get

    case frontier of
        [] -> put (DS field frontier traversed)
        ((prio, c):rest) -> do
            let traversed' = insert c prio traversed
            let nbs = restrictHeight field c $ boundTo field $ neighbors c 
            let allowed_nbs = [nb | nb <- nbs, notMember nb traversed' || (traversed' ! nb) > prio + 1]
            let traversed'' = foldr (\nb m -> insert nb (prio + 1) m) traversed' allowed_nbs

            let frontier' = sort $ fmap (prio + 1,) allowed_nbs ++ rest
            put (DS field frontier' traversed'')

while :: (s -> Bool) -> State s () -> s -> s
while pred s init = if pred init then while pred s (execState s init) else init
until' pred = while (not . pred)

dijkstraUntil p = until' (\(DS _ frontier traversed) -> null frontier || p `member` traversed) dijkstra
dijkstraAll = until' (\(DS _ frontier _) -> null frontier) dijkstra
