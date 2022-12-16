import Control.Monad.Trans.State
import Data.List
import Data.Map (Map, (!), elems)
import qualified Data.Map
import Data.Maybe
import Text.Read (readMaybe)
import Control.Monad

main = do
    input <- readFile "inputs/7.txt"

    let state = (foldl1' (>>) . parseInsts . tail . lines) input
    let (tree, _) = execState state (emptyDir, [])

    print $ sum $ allDirSizesWith (<= 100000) tree
    print $ minimum $ allDirSizesWith (>= 8381165) tree

data Dir = Dir String (Map String Dir) | File String Integer
    deriving Show
type Path = [String]

emptyDir = emptyDir' ""
emptyDir' s = Dir s Data.Map.empty

-- Index a directory
dir (p:px) (Dir _ subs) = dir px (subs ! p)
dir (p:px) f = error "file is not a directory"
dir [] d = d

-- compute directory size
dirSize (Dir _ subs) = sum $ fmap dirSize subs
dirSize (File _ size) = size

-- find paths of all directories that meet a size predicate
allDirsWith :: (Integer -> Bool) -> Dir -> [Path]
allDirsWith p (File fname size) = []
allDirsWith p d @ (Dir dname subs) = [[dname] | p $ dirSize d] ++ fmap (++ [dname]) (elems subs >>= allDirsWith p)
-- find paths of all directories that meet a size predicate

allDirSizesWith p d @ (Dir _ subs) = [size | p size] ++ (elems subs >>= allDirSizesWith p)
    where size = dirSize d
allDirSizesWith _ _ = []

-- get name of dir or file
dirName (File name _) = name
dirName (Dir name _) = name
-- insert file to directory
insertFile p d (File _ _) = error "cannot insert into file"
insertFile [] d (Dir name m) = Dir name (Data.Map.insert (dirName d) d m)
insertFile (p:px) d (Dir name m) = Dir name (Data.Map.adjust (insertFile px d) p m)

type ShellState = (Dir, Path)

cd :: Maybe String -> State ShellState ()
cd (Just d) = do
    (tree, pwd) <- get

    let tree' = insertFile pwd (emptyDir' d) tree
    let pwd' = pwd ++ [d]

    put (tree', pwd')
cd Nothing = do
    (tree, pwd) <- get
    let pwd' = init pwd

    put (tree, pwd')

ls :: [Dir] -> State ShellState ()
ls results = do
    (tree, pwd) <- get
    let tree' = foldr ($) tree $ fmap (insertFile pwd) results

    put (tree', pwd)

parseInsts lines = case takeLines lines of
    ([], [])  -> []
    ([], _) -> error "unreachable"
    (i:ix, rest)
        | "$ cd" `isPrefixOf` i -> cd (filterMaybe (/= "..") $ stripPrefix "$ cd " i) : parseInsts rest
        | "$ ls" `isPrefixOf` i -> ls (mapMaybe parseLsFile ix) : parseInsts rest
        | otherwise -> error "invalid instruction"
    where
        takeLines (l:lx) = let (left, right) = break (isPrefixOf "$") lx in (l:left, right)
        takeLines [] = ([], [])

        parseLsFile l = let (left, ' ':right) = break (== ' ') l in do
            size <- readMaybe left
            return $ File right size

        filterMaybe p (Just s) = fmap (const s) (guard (p s))
        filterMaybe _ Nothing = Nothing

stripPrefix' p = fromJust . stripPrefix p