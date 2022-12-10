{-# LANGUAGE TupleSections #-}
import Control.Monad.Trans.State;

main = do
    input <- readFile "inputs/8.txt"
    let m = matrix input :: [[Int]]
    let rows = length m
    let cols = length $ head m

    let innerVisibles = foldr dedup [] [travRows m, travCols m, fmap (\(a, b) -> (rows - a, b)) (travRows (flipRows m)), fmap (\(a, b) -> (a, cols - b)) (travCols (flipCols m))]
    
    print $ (2 * rows + 2 * cols - 4) + length innerVisibles

matrix input = do
    line <- lines input
    return $ fmap (\c -> read [c]) line

transpose ([]:_) = []
transpose m = fmap head m : transpose (fmap tail m)

flipRows :: [[a]] -> [[a]]
flipRows = reverse

flipCols = fmap reverse

travSeq (edge:inner) = findInnerVisibles edge 1 inner
    where
        findInnerVisibles _ _ [] = error "how did you get here"
        findInnerVisibles _ _ [_] = []
        findInnerVisibles v c (f:row) =
            if v < f
            then c : findInnerVisibles f (c + 1) row
            else findInnerVisibles v (c + 1) row
travSeq [] = error "seq empty"

travRow r row = (r, ) <$> travSeq row
travCol c col = (, c) <$> travSeq col

dedup a b = a ++ filter (`notElem` a) b

travRows [] = error "seq empty"
travRows (_:inner) = travInnerRows 1 inner
    where
        travInnerRows _ [] = error "how did you get here"
        travInnerRows _ [_] = []
        travInnerRows i (f:rest) = dedup (travRow i f) (travInnerRows (i + 1) rest)

travCols m = fmap (\(a, b) -> (b, a)) (travRows $ transpose m)