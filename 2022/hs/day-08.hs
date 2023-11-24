import Control.Monad.Trans.State;
import Data.Char (digitToInt)
import Control.Monad
import Data.List (transpose)
import qualified Data.Set as Set

main = do
    input <- readFile "inputs/8.txt"
    let m = matrix input
    let rows = length m
    let cols = length $ head m

    -- part A

    -- I'm leaving my prev solution here just to emphasize, how FUCKING beautiful the new solution is.
    -- Previously I only had the travSeq method, which traverses a sequence to see visible trees.
    -- So my previous solution was to transform the matrix, and use zips to keep track of the coordinates.

    -- let rfwdVisible = fmap (\(i, r) -> zipTrav (fmap (i,) [0..]) r) (zip [0..] m)
    -- let rbwdVisible = fmap (\(i, r) -> zipTrav (fmap (i,) (bwd cols)) r) (zip [0..] (rr m))
    -- let cfwdVisible = fmap (\(j, c) -> zipTrav (fmap (,j) [0..]) c) (zip [0..] (transpose m))
    -- let cbwdVisible = fmap (\(j, c) -> zipTrav (fmap (,j) (bwd rows)) c) (zip [0..] ((rr . transpose) m))

    -- The new solution:
    -- Create an INFINITE MATRIX that consists of all the coordinates.
    -- To create every pass, transform the tree matrix, apply a "forward pass", and untransform the tree matrix
    -- Mark against the coordinate matrix.
    -- THIS IS SUCH A GREAT SOLUTION BECAUSE THE COORDINATE MATRIX STAYS THE SAME!!! LIKE !!!!!!!!!

    let passes = [
            compressMatrix coords . travFr, -- forward row pass
            compressMatrix coords . travBr, -- backward row pass
            compressMatrix coords . travFc, -- forward col pass
            compressMatrix coords . travBc -- backward col pass
            ]

    -- fmap (Set.fromList . ($ m)) passes = for all the passes, apply to m, then convert result into list
    -- foldr Set.union Set.empty $ ... = merge sets via union
    let all = foldr Set.union Set.empty $ fmap (Set.fromList . ($ m)) passes
    print $ Set.size all

    -- part B
    -- Reduce coords to just the matrix length, and flatten
    let coords' = take cols =<< take rows coords

    -- straightforward
    print $ maximum $ fmap (`scenicScore` m) coords'

matrix input = do
    line <- lines input
    return $ fmap digitToInt line

travSeq :: [Int] -> [Bool]
travSeq [] = []
travSeq (t:tx) = True : checkVisible t tx
    where
        checkVisible tallest [] = []
        checkVisible tallest trees = case break (> tallest) trees of
            (hidden, []) -> fmap (const False) hidden
            (hidden, t:tx) -> fmap (const False) hidden ++ True : checkVisible t tx

-- Does a forward row pass through matrix
travFr :: [[Int]] -> [[Bool]]
travFr = fmap travSeq

-- Does a backward row pass through matrix
travBr = revRows . travFr . revRows

-- Does a forward col pass through matrix
travFc = transpose . travFr . transpose

-- Does a backward col pass through matrix
travBc = transpose . travBr . transpose

revRows :: (Functor f) => f [a] -> f [a]
revRows = fmap reverse

compressMatrix :: [[b]] -> [[Bool]] -> [b]
compressMatrix m c = do
    (mr, cr) <- zip m c
    (me, ce) <- zip mr cr
    guard ce
    return me

coords = do
    r <- [0..]
    return $ do
        c <- [0..]
        return (r, c)

-- part B

-- Create axes that indicate all the possible directions one can see from a given tree
-- List will not contain the center node
axes (r, c) m = [
    left,
    right,
    up,
    down
    ]
    where
        row = m !! r
        (left, right) = case splitAt c row of
            sp @ (l, []) -> sp -- shouldn't happen, but y'know safety check
            (l, _:rx) -> (reverse l, rx)

        m' = transpose m
        col = m' !! c
        (up, down) = case splitAt r col of
            sp @ (u, []) -> sp -- shouldn't happen, but y'know safety check
            (u, _:dx) -> (reverse u, dx)

-- Calculate scenic score at a given point
scenicScore (r, c) m = product $ fmap scen (axes (r, c) m)
    where
        tree = m !! r !! c
        -- the number of trees is the number of trees < the height of the center one AND
        -- if there's more trees beyond that, then that the one in front is visible too
        scen s = length visibles + fromEnum ((not . null) other)
            where
                (visibles, other) = span (< tree) s