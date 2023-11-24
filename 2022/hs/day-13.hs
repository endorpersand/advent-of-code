import Text.Read (readMaybe)
import Data.List (stripPrefix, sort)
import Data.Maybe (fromJust)
import Control.Monad.Trans.State
import Control.Monad

main = do
    input <- readFile "inputs/13.txt"

    let pairs = do 
        a:b:_ <- chunk 3 (lines input)
        let a' = makeTree a :: Tree Int
        let b' = makeTree b :: Tree Int
        
        return (a', b')
    
    -- print $ zip[0..] pairs !! 150
    print $ sum $ fmap (\(a, b) -> if b then a else 0) (zip [1..] (fmap (uncurry (<)) pairs))
    
    let k1 = makeTree "[[2]]" :: Tree Int
    let k2 = makeTree "[[6]]" :: Tree Int
    
    let elems = k1 : k2 : do
        (a, b) <- pairs
        [a, b]
    
    let elems' = sort elems
    print $ find k1 elems' * find k2 elems'

data Tree t = List [Tree t] | Elem t
    deriving (Show, Eq)

instance Ord t => Ord (Tree t) where
    compare (List a) (List b) = compare a b
    compare (Elem a) (Elem b) = compare a b
    compare (List a) b = compare a [b]
    compare a (List b) = compare [a] b

stripPrefix' p = fromJust . stripPrefix p

-- list: [(tree),*]
-- tree: list | el

makeTree :: Read t => String -> Tree t
makeTree = fromJust . evalState popElement

popElement :: Read t => State String (Maybe (Tree t))
popElement = do
    str <- get
    case stripPrefix "[" str of
        Just rest -> do
            put rest
            tpl <- consumeTuple

            rest <- get
            put $ stripPrefix' "]" rest
            
            return $ Just $ List tpl
        Nothing -> do
            let (num, rest) = break (`elem` ",]") str
            put rest
            case readMaybe num of
                Just t -> return $ Just $ Elem t
                Nothing -> return Nothing

consumeTuple :: Read t => State String [Tree t]
consumeTuple = do
    me <- popElement
    case me of
        Just e -> do
            rest <- get
            case stripPrefix "," rest of
                Just rest' -> do
                    put rest'
                    lst <- consumeTuple
                    return (e:lst)
                Nothing -> return [e]
        Nothing -> return []

chunk n [] = []
chunk n l = take n l : chunk n (drop n l)

find e l = head $ do
    (i, el) <- zip [1..] l
    guard (el == e)
    return i