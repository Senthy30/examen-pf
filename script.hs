{-
factori :: Int -> [Int]
factori n = [x | x <- [1..n], mod n x == 0]

prim :: Int -> Bool
prim n = length (factori n) <= 2 

numerePrime :: Int -> [Int]
numerePrime n = [x | x <- [2..n], prim x == True]

myzip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
myzip3 l1 l2 l3 = [(x, y, z) | ((x, y), z) <- zip (zip l1 l2) l3]

firstEl :: [(a, b)] -> [a]
firstEl l = map fst l

sumList :: [[Int]] -> [Int]
sumList = map sum 

prel1 :: Int -> Int
prel1 n = if mod n 2 == 0
          then div n 2
          else 2 * n

prel2 :: [Int] -> [Int]
prel2 l = map prel1 l

find1 :: Char -> [String] -> [String]
find1 n s = filter (n `elem`) s

pow1 :: [Int] -> [Int]
pow1 l = map (^2) (filter odd l)

checkodd :: (Int, Int) -> Bool
checkodd (a, b) = mod b 2 == 1

pow2 :: [Int] -> [Int]
pow2 l = map (^2) (map fst (filter checkodd (zip l [1..])))

eVocala :: Char -> Bool
eVocala ch = if ch `elem` "aeiouAEIOU"
             then True
             else False

numaiVocale :: [String] -> [String]
numaiVocale l = map (filter eVocala) l

powsum :: [Int] -> Int
powsum l = foldr (+) 0 (map (^2) (filter odd l))

checkTrue :: [Bool] -> Bool
checkTrue l = foldr (&&) True l

allVerifiesF :: (Int -> Bool) -> Bool -> Int -> Bool
allVerifiesF f b n = b && f n

allVerifies :: (Int -> Bool) -> [Int] -> Bool
--allVerifies f l = foldr (&&) True (map f l)
allVerifies f l = foldl (allVerifiesF f) True l

listToIntF :: Integer -> Integer -> Integer
listToIntF n x = n * 10 + x

listToInt :: [Integer] -> Integer
listToInt l = foldl (listToIntF) 0 l

rmChar :: Char -> String -> String
rmChar ch s = [c | c <- s, c /= ch]

rmCharsRec :: String -> String -> String
rmCharsRec [] s = s
rmCharsRec (x:xs) s = rmCharsRec xs (rmChar x s) 

rmCharsFold :: String -> String -> String
rmCharsFold x s = foldr rmChar s x

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f l = foldr (\x result -> f x : result) [] l

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr f l = foldr (\x result -> if f x == True then x : result else result) [] l

data Fruct = Mar String Bool
           | Portocala String Int

listaFructe = [Mar "Ionatan" False,
            Portocala "Sanguinello" 10,
            Portocala "Valencia" 22,
            Mar "Golden Delicious" True,
            Portocala "Sanguinello" 15,
            Portocala "Moro" 12,
            Portocala "Tarocco" 3,
            Portocala "Moro" 12,
            Portocala "Valencia" 2,
            Mar "Golden Delicious" False,
            Mar "Golden" False,
            Mar "Golden" True]

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Mar _ _) = False
ePortocalaDeSicilia (Portocala s _) = s `elem` ["Tarocco", "Moro", "Sanguinello"]

sumFeliiSicilia :: Fruct -> Int
sumFeliiSicilia (Mar _ _) = 0
sumFeliiSicilia (Portocala _ n) = n

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia l =  sum (map sumFeliiSicilia (filter ePortocalaDeSicilia l) )

areVierme :: Fruct -> Bool
areVierme (Portocala _ _) = False
areVierme (Mar _ False) = False
areVierme (Mar _ True) = True

nrMereViermi :: [Fruct] -> Int
nrMereViermi l = length (filter areVierme l)

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

vorbeste :: Animal -> String
vorbeste (Caine _ _) = "Woof!"
vorbeste (Pisica _) = "Meow!"

rasa :: Animal -> Maybe String
rasa (Pisica _) = Nothing
rasa (Caine _ r) = Just r

data Linie = L [Int]
    deriving Show
data Matrice = M [Linie]
    deriving Show

verifica :: Matrice -> Int -> Bool
verifica (M m) n = foldr (\(L list) result -> result && (sum list == n)) True m

checkElemente :: Linie -> Int -> Bool
checkElemente (L list) n = if length list == n
                           then length (filter (>0) list) == n
                           else True

doarPozN :: Matrice -> Int -> Bool
doarPozN (M m) n = foldr (\(L list) result -> result && checkElemente (L list) n) True m

checkLength :: Int -> Int -> Int
checkLength n (-2) = n
checkLength (-1) n = -1
checkLength n x = if n == x then n else -1

func :: Linie -> Int -> Int
func (L list) n = checkLength (length list) n 

corect :: Matrice -> Bool
--corect (M m) = if (foldr func (-2) m) == -1 then False else True
corect (M m) = if foldr (\(L line) result -> checkLength (length line) result) (-2) m == -1 then False else True

data Nat = Zero | Succ Nat
    deriving Show

(+++ ) :: Nat -> Nat -> Nat
m +++ Zero = m
m +++ ( Succ n ) = Succ (m +++ n )

-}





{-
data Expr = Const Int -- integer constant
          | Expr :+: Expr -- addition
          | Expr :*: Expr -- multiplication
           deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
          | Node Operation Tree Tree -- branch
           deriving (Eq, Show)

instance Show Expr where
    show :: Expr -> String
    show (Const i) = show i
    show (e1 :+: e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
    show (e1 :*: e2) = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"

evalExp :: Expr -> Int
evalExp (Const x) = x
evalExp (x1 :+: x2) = (evalExp x1) + (evalExp x2)
evalExp (x1 :*: x2) = (evalExp x1) * (evalExp x2)

exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)
test11 = evalExp exp1 == 6
test12 = evalExp exp2 == 14
test13 = evalExp exp3 == 13
test14 = evalExp exp4 == 16

evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb (Node Add x1 x2) = (evalArb x1) + (evalArb x2)
evalArb (Node Mult x1 x2) = (evalArb x1) * (evalArb x2)


arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)

test21 = evalArb arb1 == 6
test22 = evalArb arb2 == 14
test23 = evalArb arb3 == 13
test24 = evalArb arb4 == 16


expToArb :: Expr -> Tree
expToArb = undefined


class Collection c where
    empty :: c key value
    singleton :: key -> value -> c key value
    insert
        :: Ord key
        => key -> value -> c key value -> c key value
    clookup :: Ord key => key -> c key value -> Maybe value
    delete :: Ord key => key -> c key value -> c key value
    keys :: c key value -> [key]
    values :: c key value -> [value]
    toList :: c key value -> [(key, value)]
    fromList :: Ord key => [(key,value)] -> c key value

newtype PairList k v
    = PairList { getPairList :: [(k, v)] }


data SearchTree key value
    = Empty
    | BNode
        (SearchTree key value) -- elemente cu cheia mai mica
        key                    -- cheia elementului
        (Maybe value)          -- valoarea elementului
        (SearchTree key value) -- elemente cu cheia mai mare

data Cow = Cow { name :: String , age :: Int , weight :: Int} 
    deriving (Eq, Show)



noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s = Just s

noNegative :: Int -> Maybe Int
noNegative x = if x < 0 then Nothing else Just x

-}




{-
sumPow2 :: [Integer] -> Integer
sumPow2 l = foldl (+) 0 (map (^2) l)

allTrue :: [Bool] -> Bool
allTrue l = foldl (&&) True l

listToIntF :: Integer -> Integer -> Integer
listToIntF x n = x * 10 + n

listToInt :: [Integer] -> Integer
listToInt l = foldl listToIntF 0 l

rmChar :: Char -> String -> String
rmChar ch s = [c | c <- s, c /= ch]

rmCharsRec :: String -> String -> String
rmCharsRec [] s = s
rmCharsRec (x:xs) s = rmCharsRec xs (rmChar x s)

rmCharsFold :: String -> String -> String
rmCharsFold s1 s2 = foldr (rmChar) s2 s1

data Linie = L [Int]
    deriving Show
data Matrice = M [Linie]
    deriving Show

sumOnLinie :: Int -> Linie -> Bool
sumOnLinie n (L l) = (foldl (+) 0 l) == n

verifica :: Matrice -> Int -> Bool
verifica (M l) n = foldl (&&) True (map (sumOnLinie n) l)

data Expr = Const Int -- integer constant
    | Expr :+: Expr -- addition
    | Expr :*: Expr -- multiplication
    deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
    | Node Operation Tree Tree -- branch
    deriving (Eq, Show)

evalExp :: Expr -> Int
evalExp (Const n) = n
evalExp (a :+: b) = evalExp a + evalExp b 
evalExp (a :*: b) = evalExp a * evalExp b

evalArb :: Tree -> Int
evalArb (Lf n) = n
evalArb (Node Add a b) = evalArb a + evalArb b
evalArb (Node Mult a b) = evalArb a * evalArb b 

expToArb :: Expr -> Tree
expToArb (Const n) = (Lf n)
expToArb (a :+: b) = (Node Add (expToArb a) (expToArb b)) 
expToArb (a :*: b) = (Node Mult (expToArb a) (expToArb b))

class Collection c where
    empty :: c key value
    singleton :: key -> value -> c key value
    insert:: Ord key => key -> value -> c key value -> c key value
    clookup:: Ord key => key -> c key value -> Maybe value
    delete :: Ord key => key -> c key value -> c key value
    keys :: c key value -> [key]
    keys collection = [fst x | x <- toList collection]
    values :: c key value -> [value]
    values collection = [snd x | x <- toList collection] 
    toList :: c key value -> [(key, value)]
    fromList :: Ord key => [(key, value)] -> c key value
    fromList = foldr (uncurry insert) empty

newtype PairList k v
    = PairList { getPairList :: [(k, v)] }
    deriving Show

instance Collection PairList where
    empty = PairList []
    singleton a b = PairList [(a, b)]
    insert a b (PairList []) = PairList [(a, b)]
    insert a b (PairList ((k, v) : xs))
        | a < k = PairList ((a, b) : (k, v) : xs)
        | a == k = PairList ((a, b) : xs)
        | a > k = let PairList l = insert a b (PairList xs) in PairList((k, v) : l) 
    clookup a (PairList []) = Nothing
    clookup a (PairList ((k, v) : xs))
        | a == k = Just v
        | a > k = clookup a (PairList xs)
    delete a (PairList []) = (PairList [])
    delete a (PairList ((k, v) : xs)) 
        | a == k = (PairList xs)
        | otherwise = let PairList l = delete a (PairList xs) in PairList((k, v) : l) 
    toList = getPairList


data SearchTree key value
    = Empty
    | BNode
    (SearchTree key value) -- elemente cu cheia mai mica
    key -- cheia elementului
    (Maybe value) -- valoarea elementului
    (SearchTree key value) -- elemente cu cheia mai mare
    deriving Show

instance Collection SearchTree where
    empty = Empty
    singleton key value = (BNode Empty key (Just value) Empty)
    insert key value Empty = (BNode Empty key (Just value) Empty)
    insert key value (BNode arbLeft k v arbRight)
        | key < k = (BNode (insert key value arbLeft) k v arbRight)
        | key == k = (BNode arbLeft key (Just value) arbRight)
        | key > k = (BNode arbLeft k v (insert key value arbRight))
    clookup key Empty = Nothing
    clookup key (BNode arbLeft k v arbRight)
        | key == k = v
        | key < k = clookup key arbLeft
        | key > k = clookup key arbRight
    delete key (BNode arbLeft k v arbRight)
        | key == k = (BNode arbLeft k Nothing arbRight)
        | key < k = delete key arbLeft
        | key > k = delete key arbRight
    toList Empty = []
    toList (BNode arbLeft key (Just value) arbRight) = toList arbLeft ++ [(key, value)] ++ toList arbRight
    toList (BNode arbLeft _ Nothing arbRight) = toList arbLeft ++ toList arbRight

data Punct = Pt [Int]

instance Show Punct where
    show (Pt lst) = let 
                        repl '[' = '('
                        repl ']' = ')'
                        repl x = x
                    in map repl (show lst)

data Arb = Vid | F Int | N Arb Arb
    deriving Show

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

instance ToFromArb Punct where
    toArb (Pt []) = Vid
    toArb (Pt (x:xs)) = N (F x) (toArb (Pt xs))
    fromArb (Vid) = Pt []
    fromArb (F n) = Pt [n]
    fromArb (N arbLeft arbRight) =  let 
                                        Pt l = fromArb arbLeft 
                                        Pt r = fromArb arbRight 
                                    in (Pt (l ++ r))


data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
    perimeter :: (Floating a) => g a -> a
    area :: (Floating a) => g a -> a

instance GeoOps Geo where
    perimeter (Square d) = 4 * d
    perimeter (Rectangle d w) = 2 * (d + w)
    perimeter (Circle r) = 2 * pi * r
    area (Square d) = d * d
    area (Rectangle d w) = d * w
    area (Circle r) = pi * r * r

instance (Floating a, Eq a) => Eq (Geo a) where
    (Square b1) == (Square b2) = (perimeter (Square b1)) == (perimeter (Square b2))
    (Square b1) == (Rectangle a2 b2) = (perimeter (Square b1)) == (perimeter (Rectangle a2 b2))
    (Square b1) == (Circle b2) = (perimeter (Square b1)) == (perimeter (Circle b2))
    (Rectangle a1 b1) == (Square a2) = (perimeter (Rectangle a1 b1)) == (perimeter (Square a2))
    (Rectangle a1 b1) == (Rectangle a2 b2) = (perimeter (Rectangle a1 b1)) == (perimeter (Rectangle a2 b2))
    (Rectangle a1 b1) == (Circle a2) = (perimeter (Rectangle a1 b1)) == (perimeter (Circle a2))
    (Circle r1) == (Square r2) = (perimeter (Circle r1)) == (perimeter (Square r2))
    (Circle r1) == (Rectangle a2 b2) = (perimeter (Circle r1)) == (perimeter (Rectangle a2 b2))
    (Circle r1) == (Circle r2) = (perimeter (Circle r1)) == (perimeter (Circle r2))


data CountingBad a =
    Heisenberg Int a
    deriving (Eq, Show)

instance Functor CountingBad where
    fmap f ( Heisenberg n a ) = Heisenberg (n + 1) ( f a )


newtype Identity a = Identity a
instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a
    deriving Show
instance Functor Pair where
    fmap f (Pair b c) = Pair (f b) (f c)

data Constant a b = Constant b
    deriving Show
instance Functor (Constant a) where
    fmap f (Constant b) = Constant (f b)


data Two a b = Two a b
    deriving Show
instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c
    deriving Show
instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b
    deriving Show
instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

data Four a b c d = Four a b c d
    deriving Show
instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

data Four'' a b = Four'' a a a b
    deriving Show
instance Functor (Four'' a) where
    fmap f (Four'' a b c d) = Four'' a b c (f d)

data Quant a b = Finance | Desk a | Bloor b | Ceva b
    deriving Show
instance Functor (Quant a) where
    fmap f Finance = Finance
    fmap f (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)
    fmap f (Ceva b) = Ceva (f b)


data LiftItOut f a = LiftItOut (f a)
    deriving Show
instance Functor f => Functor (LiftItOut f) where
    fmap g (LiftItOut fv) = LiftItOut (fmap g fv)

data Parappa f g a = DaWrappa (f a) (g a)
    deriving Show
instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap h (DaWrappa fv gv) = DaWrappa (fmap h fv) (fmap h gv) 

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
    deriving Show
instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
    fmap h (IgnoringSomething fv gv) = IgnoringSomething fv (fmap h gv)

data Notorious g o a t = Notorious (g o) (g a) (g t)
    deriving Show
instance Functor g => Functor (Notorious g o a) where
    fmap h (Notorious fv gv hv) = Notorious fv gv (fmap h hv)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
    deriving Show
instance Functor GoatLord where
    fmap f NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats fv gv hv) = MoreGoats (fmap f fv) (fmap f gv) (fmap f hv)

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
    fmap f Halt = Halt
    fmap f (Print x fv) = Print x (f fv)
    fmap f (Read fv) = Read (f . fv)


-- lab 11

data List a = Nil | Cons a (List a)
    deriving (Eq, Show)
instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x fv) = Cons (f x) (fmap f fv)
    
instance Applicative List where
    pure x = Cons x Nil
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Cons f fs) xs = append (fmap f xs) (fs <*> xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)
test1 = (f <*> v) == Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))


data Cow = Cow { name :: String, age :: Int, weight :: Int} 
    deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty s = if (length s) /= 0 then (Just s) else Nothing

noNegative :: Int -> Maybe Int
noNegative n = if n > 0 then (Just n) else Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString nm ag wg = if noEmpty nm /= Nothing && noNegative ag /= Nothing && noNegative wg /= Nothing then 
                            Just (Cow nm ag wg)
                         else Nothing

cowFromStringV2 :: String -> Int -> Int -> Maybe Cow
cowFromStringV2 n a w = Cow <$> noEmpty n <*> noNegative a <*> noNegative w



newtype Name = Name String 
    deriving (Eq, Show)
newtype Address = Address String 
    deriving (Eq, Show)
data Person = Person Name Address
    deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength a b
    | length b < a      = Just b
    | otherwise          = Nothing

mkName :: String -> Maybe Name
mkName a
    | validateLength 25 a == Nothing    = Nothing
    | otherwise                         = Just (Name a)

mkAddress :: String -> Maybe Address
mkAddress a
    | validateLength 100 a == Nothing    = Nothing
    | otherwise                         = Just (Address a)

mkPerson :: String -> String -> Maybe Person
mkPerson nume adresa = Person <$> mkName nume <*> mkAddress adresa

-- laboratorul 12

-}

{-

import Control.Monad (join)
import Data.Monoid

{-
data BinaryTree a = Leaf a | Node ( BinaryTree a ) ( BinaryTree a )
    deriving Show

foldTree :: ( a -> b -> b ) -> b -> BinaryTree a -> b
foldTree f i ( Leaf x ) = f x i
foldTree f i (Node l r ) = foldTree f ( foldTree f i l ) r

myTree = Node (Node (Leaf 1) (Leaf 2) ) (Node (Leaf 3) (Leaf 4) )

instance Foldable BinaryTree where
    foldr f i (Leaf x) = f x i
    foldr f i (Node l r ) = foldTree f ( foldTree f i l ) r

-}


elem1 :: (Foldable t, Eq a) => a -> t a -> Bool
elem1 x = foldr (\y acc -> acc || x == y) False

null1 :: (Foldable t) => t a -> Bool
null1 = foldr (\_ _ -> False) True

length1 :: (Foldable t) => t a -> Int
length1 = foldr (\y result -> result + 1) 0

toList1 :: (Foldable t) => t a -> [a]
toList1 = foldr (:) []

fold1 :: (Foldable t, Monoid m) => t m -> m
fold1 = foldMap id


data Constant a b = Constant b
    deriving Show
instance Foldable (Constant a) where
    foldr f i (Constant b) = f b i

data Two a b = Two a b
    deriving Show
instance Foldable (Two a) where
    foldr f i (Two _ b) = f b i

data Three a b c = Three a b c
    deriving Show
instance Foldable (Three a b) where
    foldr f i (Three _ _ c) = f c i

data Three' a b = Three' a b b
    deriving Show
instance Foldable (Three' a) where
    foldr f i (Three' _ a b) = f a (f b i) 

data Four' a b = Four' a b b b
    deriving Show
instance Foldable (Four' a) where
    foldr f i (Four' _ a b c) = f a (f b (f c i))

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
    deriving Show
instance Foldable GoatLord where
    foldr _ i NoGoat = i
    foldr f i (OneGoat a) = f a i
    foldr f i (MoreGoats g1 g2 g3) = foldr f (foldr f (foldr f i g1) g2) g3

binding :: IO ( )
binding = do
    name <- getLine
    putStrLn name

twoBinds :: IO()
twoBinds = putStrLn "name: " >> getLine >>= \name -> putStrLn "age: " >> getLine >>= \age -> putStrLn ("hello " ++ name ++ ", your age is " ++ age)

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x * x, x * x]
    else [x * x]


pos :: Int -> Bool
pos x = if (x>=0) then True else False

fct :: Maybe Int -> Maybe Bool
fct mx = do
    x <- mx
    return (pos x)

addM :: Maybe Int -> Maybe Int -> Maybe Int
addM mx my = mx >>= \x1 -> my >>= \x2 -> return (x1 + x2)
{-
addM mx my = do
    x1 <- mx
    x2 <- my
    return (x1 + x2)
-}

cartesian_product :: Maybe Int -> Maybe Int -> Maybe (Int, Int)
cartesian_product mx my = do
    x <- mx
    y <- my
    return (x, y)
--cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))

prod :: (Maybe Int -> Maybe Int -> Maybe Int) -> [Maybe Int] -> [Maybe Int] -> [Maybe Int]
prod f xs ys = xs >>= \x -> ys >>= \y -> return (f x y)
{-
prod f xs ys = do
    x <- xs
    y <- ys
    return (f x y)
-}
--prod f xs ys = [f x y | x <- xs, y<-ys]

myGetLine :: IO String
myGetLine = do
    x <- getChar
    if x == '\n' 
        then return []
    else do
        xs <- myGetLine
        return (x:xs)
{-
myGetLine = getChar >>= \x ->
    if x == '\n' 
        then return []
    else
        myGetLine >>= \xs -> return (x:xs)
-}

--- Monada Writer

newtype WriterS a = Writer { runWriter :: (a, String) } 

instance  Monad WriterS where
    return va = Writer (va, "")
    ma >>= k = let (va, log1) = runWriter ma
                    (vb, log2) = runWriter (k va)
                in  Writer (vb, log1 ++ log2)

instance  Applicative WriterS where
    pure = return
    mf <*> ma = do
        f <- mf
        a <- ma
        return (f a)       

instance  Functor WriterS where              
    fmap f ma = pure f <*> ma     

tell :: String -> WriterS () 
tell log = Writer ((), log)
  
logIncrement :: Int  -> WriterS Int
logIncrement x = undefined

logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x n =  undefined

-}

{-

data Point = Pt [Int]
    deriving Show

data Arb = Empty | Node Int Arb Arb
    deriving Show

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

instance ToFromArb Point where
    fromArb Empty = (Pt [])
    fromArb (Node val arbLeft arbRight) = let 
                                            (Pt l) = fromArb arbLeft
                                            (Pt r) = fromArb arbRight
                                            in
                                            Pt (l ++ [val] ++ r)
    toArb (Pt []) = Empty
    toArb (Pt (x:xs)) = (Node x (toArb (Pt (filter (<= x) xs))) (toArb (Pt (filter (> x) xs)))) 


getFromInterval :: Int -> Int -> [Int] -> [Int]
getFromInterval a b l = l >>= (\x -> 
    if x >= a && x <= b then
        return x
    else [] )
--getFromInterval a b l = [x | x <- l, x >= a && x <= b]

getFromInterval a b l = do
    x <- l
    if x >= a && x <= b then
        return x   
    else []
    -}

newtype ReaderWriter env a = RW { getRW :: env-> (a,String) }

instance Monad (ReaderWriter env) where
    return x = RW (\_ -> (x, ""))
    mx >>= g = RW f
        where f env = let 
                        (a, log1) = getRW mx env
                        (b, log2) = getRW (g a) env
                      in 
                        (b, log1 ++ log2) 
    {-
    (RW f) >>= g = RW (\env ->  let 
                                    (a, log1) = f env
                                    (RW h) = g a
                                    (b, log2) = h env
                                in 
                                    (b, log1 ++ log2))
    -}

instance Applicative (ReaderWriter env) where
    pure = return
    mf <*> mx = do
        f <- mf
        x <- mx
        return (f x)

instance Functor (ReaderWriter env) where
    fmap f x = pure f <*> x

f1 :: Int -> ReaderWriter Int Bool
f1 n = RW (\_ -> (n > 0, "The value is positive\n"))

g1 :: Bool -> ReaderWriter Int String
g1 b = RW (\env -> (show env, "The environment is " ++ show env ++ "\n"))

{-
newtype TestMonad env a = MD {getMD :: env -> (a, String, String, String)}

instance Monad (TestMonad env) where
    return x = MD (\_ -> (x, "", "", ""))
    mx >>= g = MD f
        where f env = let
                        (a, last11, last12, log1) = getMD mx env
                        (b, last21, last22, log2) = getMD (g a) env
                      in
                        (b, log1, log2, log1 ++ log2)
                
instance Applicative (TestMonad env) where
    pure = return
    mf <*> mx = do
        f <- mf
        x <- mx
        return (f x)

instance Functor (TestMonad env) where
    fmap f x = pure f <*> x

f1 :: Int -> ReaderWriter Int Bool
f1 n = RW (\_ -> (n > 0, "The value is positive\n"))

g1 :: Bool -> ReaderWriter Int String
g1 b = RW (\env -> (show env, "The environment is " ++ show env ++ "\n"))

f2 :: Int -> TestMonad Int Bool
f2 n = MD (\_ -> (n > 0, "", "", "The value is positive!\n"))

g2 :: Bool -> TestMonad Int String
g2 b = MD (\env -> (show env, "", "", "The environment is " ++ show env ++ "\n"))

data CvMonad env a = CMD {getCMD :: env -> (a, String), getLT :: env -> (String, String)}

instance Monad (CvMonad env) where
    return x = CMD (\_ -> (x, ""))  (\_ -> ("", ""))
    mx >>= g = CMD 
        (
            \env -> let   
                        (a, log1) = getCMD mx env
                        (b, log2) = getCMD (g a) env
                    in
                        (b, log1 ++ log2)
        )
        (
            \env -> let
                (a, log1) = getCMD mx env
                (b, log2) = getCMD (g a) env
            in
                (log1, log2)
        )

instance Applicative (CvMonad env) where
    pure = return
    mf <*> mx = do
        f <- mf
        x <- mx
        return (f x)

instance Functor (CvMonad env) where
    fmap f x = pure f <*> x

f3 :: Int -> CvMonad Int Bool
f3 n = CMD (\_ -> (n > 0, show n ++ " is positive!\n")) (\_ -> ("", ""))

g3 :: Bool -> CvMonad Int Int
g3 b = CMD (\env -> (env, "The environment is " ++ show env ++ "\n")) (\_ -> ("", ""))

-}

{-
data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x l) = Cons (f x) (fmap f l) 

instance Applicative List where
    pure x = Cons x Nil
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Cons f fs) xs = append (fmap f xs) (fs <*> xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

data Cow = Cow { name :: String, age :: Int , weight :: Int, an :: Int} 
    deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty s = if (length s) /= 0 then (Just s) else Nothing

noNegative :: Int -> Maybe Int
noNegative n = if n > 0 then (Just n) else Nothing

cowFromString :: String -> Int -> Int -> Int -> Maybe Cow
cowFromString nume varsta greutate anul = Cow <$> noEmpty nume <*> noNegative varsta <*> noNegative greutate <*> noNegative anul

data LiftItOut f a = LiftItOut (f a)
    deriving Show

instance Functor f => Functor (LiftItOut f) where
    fmap g (LiftItOut mx) = LiftItOut (fmap g mx)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
    deriving Show

instance Functor GoatLord where
    fmap f NoGoat = NoGoat
    fmap f (OneGoat x) = OneGoat (f x)
    fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
    fmap f Halt = Halt
    fmap f (Print s x) = Print s (f x)
    fmap f (Read g) = Read (f . g)
-}

data Constant a b = Constant b
    deriving Show
instance Foldable (Constant a) where
    foldMap f (Constant b) = f b

data Two a b = Two a b
    deriving Show
instance Foldable (Two a) where
    foldMap f (Two x y) = f y

data Three a b = Three a b b
    deriving Show
instance Foldable (Three a) where
    foldMap f (Three x y z) = mappend (f y) (f z)

data Four a b = Four a b b b
    deriving Show
instance Foldable (Four a) where
    foldMap f (Four x y z t) = mappend (f y) (mappend (f z) (f t))

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
    deriving Show
instance Foldable GoatLord where
    foldMap f NoGoat = mempty
    foldMap f (OneGoat x) = f x
    foldMap f (MoreGoats x y z) = mappend (foldMap f x) (mappend (foldMap f y) (foldMap f z))