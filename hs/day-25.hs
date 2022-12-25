import Data.Foldable (foldl')
import qualified Data.Bimap as Bimap
import Data.Bimap ((!), (!>))

main = do
    input <- readFile "inputs/25.txt"
    let digits = [fmap (snafuDigit !) line | line <- lines input]

    putStr "sum: "
    print $ sum $ fmap (fromBase 5) digits
    
    putStr "sum-snafu: "
    putStrLn $ toSnafu $ sum $ fmap (fromBase 5) digits

snafuDigit = Bimap.fromList [
    ('2', 2),
    ('1', 1),
    ('0', 0),
    ('-', -1),
    ('=', -2)
    ]

fromBase n = foldl' (\a b -> a * n + b) 0

balQR n d = if r > (d `div` 2) then (q + 1, r - d) else (q, r)
    where (q, r) = quotRem n d

toSnafu 0 = "0"
toSnafu n = conv n []
    where 
        conv 0 = id
        conv n = conv q . ([snafuDigit !> r] ++)
            where (q, r) = balQR n 5