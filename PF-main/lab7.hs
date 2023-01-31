import Data.Binary.Get (label)
data Expr = Const Int -- integer constant
    | Expr :+: Expr -- addition
    | Expr :*: Expr -- multiplication
    deriving Eq

instance Show Expr where
    show (Const a) = show a
    show (a :+: b) = show a ++ "+" ++ show b
    show (a :*: b) = show a ++ "*" ++ show b

data Operation = Add | Mult deriving (Eq, Show)
data Tree = Lf Int -- leaf
    | Node Operation Tree Tree -- branch
    deriving (Eq, Show)


exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)

evalExp :: Expr -> Int
evalExp (Const a) = a
evalExp ( a :+: b) = evalExp a + evalExp b 
evalExp (a :*: b) = evalExp a * evalExp b


arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)

evalArb :: Tree -> Int
evalArb (Lf a) = a
evalArb (Node Add a b) = evalArb(a) + evalArb(b)
evalArb (Node Mult a b) = evalArb(a) * evalArb(b)

class Collection c where
    empty :: c key value
    singleton :: key -> value -> c key value
    insert:: Ord key => key -> value -> c key value -> c key value
    clookup:: Ord key => key -> c key value -> Maybe value
    delete :: Ord key => key -> c key value -> c key value
    keys :: c key value -> [key]
    keys collection = [fst x | x<- toList collection]
    values :: c key value -> [value]
    values collection = [snd x | x<- toList collection] 
    toList :: c key value -> [(key, value)]
    fromList :: Ord key => [(key, value)] -> c key value
    fromList = foldr (uncurry insert) empty

newtype PairList k v = PairList { getPairList :: [(k, v)] }

instance Collection PairList where
    empty = PairList []
    singleton a b = PairList [(a, b)]

    insert a b (PairList[]) = PairList[(a, b)]
    insert a b (PairList((k, v) : xs))
        | a < k = PairList((a, b) : (k, v) : xs)
        | a == k = PairList((a, b) : xs)
        | a > k = let PairList lista = insert a b (PairList xs) in PairList ((k, v) : lista)

    clookup a b = find a (getPairList b)
        where 
            find _ [] = Nothing
            find a (x: xs) =
                if a == fst(x)
                    then Just (snd x)
                else
                    find a xs

    delete a (PairList[]) = empty
    delete a (PairList ((k, v) : xs) )
        | a==k = delete a (PairList xs)
        | otherwise = let PairList lista = delete a (PairList xs) in PairList( (k, v) : lista)

    toList = getPairList

test = PairList {getPairList = [('a',5),('c',5),('b', 7)]}


data SearchTree key value
    = Empty
    | BNode
    (SearchTree key value) -- elemente cu cheia mai mica
    key -- cheia elementului
    (Maybe value) -- valoarea elementului
    (SearchTree key value) -- elemente cu cheia mai mare

instance Collection SearchTree where
    empty = Empty
    singleton a b = BNode empty a (Just b) Empty
    insert a b (BNode l key value r) 
        | a < key = BNode (insert a b l) key value r
        | a > key = BNode l key value (insert a b r)
        | a == key = BNode l key (Just b) r

    clookup _ Empty = Nothing
    clookup k (BNode l key value r)
        | k == key = value
        | k > key = clookup k l
        | k < key = clookup k r

    delete k (BNode l key value r) 
        | k == key = BNode l key Nothing r
        | k < key = BNode (delete k l) key value r 
        | k > key = BNode l key value (delete k r)
    
    toList Empty = []
    toList (BNode l key (Just value) r ) = toList l ++ [(key,value)] ++ toList r
    toList (BNode l _ Nothing r ) = toList l++ toList r  