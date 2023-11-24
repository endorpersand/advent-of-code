import qualified Data.Set as Set
import Control.Monad

main = do
    input <- readFile "inputs/18.txt"
    
    let triplets = fmap parseTriplet (lines input)
    let triplets' = Set.fromList triplets

    -- part A
    let edges = length $ do
        t <- triplets
        n <- neighbors t
        guard ((not . Set.member n) triplets')
    print edges

    -- part B
    let values = (Set.toList . Set.fromList) $ do 
        (a, b, c) <- triplets
        [a, b, c]
    let (min, max) = (minimum values - 1, maximum values + 1)
    let outer = outers min max triplets'
    -- print (min, max)
    -- print outer
    let edges = length $ do
        t <- Set.toList outer
        n <- neighbors t
        guard (Set.member n triplets')
    print edges



parseTriplet :: [Char] -> (Int, Int, Int)
parseTriplet s = let [a, b, c] = fmap read (split s) in (a, b, c)
    where
        split s = case break (== ',') s of
            (a, []) -> [a]
            (a, ',':rest) -> a : split rest
            (a, rest) -> error "unreachable"

neighbors (a, b, c) = [
    (a + 1, b, c),
    (a - 1, b, c),
    (a, b + 1, c),
    (a, b - 1, c),
    (a, b, c + 1),
    (a, b, c - 1)
    ]

outers min max blocks = scan [(min, min, min)] Set.empty
    where
        scan [] outer = outer
        scan water outer = scan (Set.toList water') (Set.union outer water')
            where 
                water' = Set.fromList $ do
                    w <- water
                    n @ (a, b, c) <- neighbors w
                    guard $ min <= a && a <= max
                    guard $ min <= b && b <= max
                    guard $ min <= c && c <= max
                    guard $ not (Set.member n blocks)
                    guard $ not (Set.member n outer)
                    return n