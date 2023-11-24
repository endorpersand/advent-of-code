import Data.Map (Map)
import qualified Data.Map as Map
import Text.Read (readMaybe)
main = do
    input <- readFile "inputs/21.txt"
    let yells = asYells input
    print $ getYell "root" yells
    print $ (solve . uncurry intoEquation . initPt2) yells

-- part A
type PendingCalc = (String, Operation, String)
data Operation = Plus | Minus | Mul | Div deriving Eq

data Yells = Yells {
    resolved :: Map String Int,
    awaiting :: Map String PendingCalc
} deriving Eq

asYells s = foldr applyLine (Yells Map.empty Map.empty) (lines s)
    where
        parseCalc s = let (l, ' ':op:' ':r) = break (== ' ') s in
            case op of
                '+' -> (l, Plus, r)
                '-' -> (l, Minus, r)
                '*' -> (l, Mul, r)
                '/' -> (l, Div, r)
                _ -> error "unimplemented"
        applyLine line (Yells resolved awaiting) = let (name, ':':' ':calc) = splitAt 4 line in
            case readMaybe calc of
                Just i -> Yells (Map.insert name i resolved) awaiting
                Nothing -> Yells resolved (Map.insert name (parseCalc calc) awaiting)

apply Plus = (+)
apply Minus = (-)
apply Mul = (*)
apply Div = div

resolveOnce (Yells resolved awaiting) = Yells (Map.union resolved resolvables) (Map.difference awaiting resolvables)
    where 
        resolve (left, op, right) = do
            lval <- Map.lookup left resolved
            rval <- Map.lookup right resolved
            return $ apply op lval rval
        resolvables = Map.mapMaybe resolve awaiting
getYell s y @ (Yells resolved _) = case Map.lookup s resolved of
    Just i -> i
    Nothing -> getYell s (resolveOnce y)

-- part B
initPt2 (Yells resolved awaiting) = (
        fixed resolveOnce (Yells (Map.delete "humn" resolved) (Map.delete "root" awaiting)), 
        let (l, _, r) = awaiting Map.! "root" in (l, r)
    )
    where
        fixed f a
            | a == f a = a
            | otherwise = fixed f (f a)

intoEquation (Yells resolved awaiting) (r0, r1) = Equation (parseLHS lhs) rhs
    where
        value t = Map.lookup t resolved
        (lhs, rhs) = case (value r0, value r1) of
            (Just lval, _) -> (r1, lval)
            (_, Just rval) -> (r0, rval)
            _ -> error "expected resolution"
        
        parseLHS "humn" = Var
        parseLHS s = let (l, op, r) = awaiting Map.! s in
            case (value l, value r) of
                (Just lval, _) -> KLeft lval op (parseLHS r)
                (_, Just rval) -> KRight (parseLHS l) op rval
                _ -> error "expected resolution"

data LHS = KLeft Int Operation LHS | KRight LHS Operation Int | Var
data Equation = Equation LHS Int

invert (KLeft a Plus _) = subtract a
invert (KLeft a Minus _) = (a - )
invert (KLeft a Mul _) = (`div` a)
invert (KLeft a Div _) = (a `div`)

invert (KRight _ Plus b) = subtract b
invert (KRight _ Minus b) = (+ b)
invert (KRight _ Mul b) = (`div` b)
invert (KRight _ Div b) = (* b)

invert Var = id

solve (Equation lhs @ (KLeft _ _ lhs') rhs) = solve (Equation lhs' (invert lhs rhs))
solve (Equation lhs @ (KRight lhs' _ _) rhs) = solve (Equation lhs' (invert lhs rhs))
solve (Equation Var rhs) = rhs