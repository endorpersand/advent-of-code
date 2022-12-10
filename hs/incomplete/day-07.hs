import Control.Monad.Trans.State
import Data.List

main = do
    input <- readFile "inputs/7.txt"
    let a = lines input
    let dir = Dir "a" [File "x" 1, Dir "y" [File "b" 1, File "c" 2, File "d" 3], File "z" 3]
    print $ allDirsWith (<=2) dir
    print $ allDirsWith (>=3) dir

data Dir = Dir String [Dir] | File String Integer
type Path = [String]

-- Index a directory
dir (p:px) (Dir _ subs) = head $ filter (\ (Dir name _) -> name == p) subs
dir (p:px) f = error "file is not a directory"
dir [] d = d

-- compute directory size
dirSize (Dir _ subs) = sum $ fmap dirSize subs
dirSize (File _ size) = size

-- find paths of all directories that meet a size predicate
allDirsWith :: (Integer -> Bool) -> Dir -> [Path]
allDirsWith p (File fname size) = [[fname] | p size]
allDirsWith p d @ (Dir dname subs) = [[dname] | p $ dirSize d] ++ fmap (++ [dname]) (subs >>= allDirsWith p)

data InstTree = Cd String [InstTree] | Ls [(String, Integer)]
