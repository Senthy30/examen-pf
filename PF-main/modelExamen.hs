import Control.Monad (foldM, mfilter)

--1
data Point = Pt [Int]
    deriving Show

data Arb = Empty | Node Int Arb Arb
    deriving Show

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

instance ToFromArb Point where
    toArb (Pt[]) = Empty
    toArb(Pt(x:xs)) = Node x (toArb(Pt(filter(<x) xs))) (toArb(Pt(filter(>=x) xs)))

    fromArb Empty = Pt[]
    fromArb (Node x st dr) = let Pt p1 = fromArb st
                                 Pt p2 = fromArb dr
                            in Pt(p1 ++ [x] ++ p2)

--2
getFromInterval x y xs = [z | z<-xs, x<=z && z<=y]

getFromInterval1 x y xs = do
    lst <- xs
    if x <= lst && lst <= y then
        return lst
    else []

--alte ex
factori :: Int -> [Int]
factori x = [ y | y <- [1..x], mod x y == 0]

--factori1 :: (Eq b, Integral [b]) => [b] -> [b]
factori1 x = do
    y <- [1..x]
    if mod x y == 0 then
        return y
    else []
    --a <- [y | y<- [1..x] , mod x y == 0]
    --return a
    
prim :: Int -> Bool
prim n = if length (factori n) == 2 then True
    else False

prim1 n = do
    x <- [1..n]
    if mod n x == 0 && x /= 1 && x/= n then
        return x
    else []

numerePrime :: Int -> [Int]
numerePrime n = [y | y<-[1..n], prim y == True]

numerePrime1 n = do
    y <- [1..n]
    if prim (y) == True then
        return y
    else []

nv = ["laboratorul", "PrgrAmare", "DEclarativa"]

vocale a = [y | y<-a, y == 'a' || y == 'e' || y == 'i' || y == 'o' || y == 'u' || y == 'A' || y == 'E' || y == 'I' || y == 'O' || y == 'U']

numaiVocale list = do
    cuv <- list
    if length (vocale cuv) /= 0 then
        return (vocale cuv)
    else []



pozitive lst = length ([y | y<-lst, y > 0])

countPositive xs = foldM (\acc x -> return (if x > 0 then acc + 1 else acc)) 0 xs

pozinlist a (x:xs) c 
    |  a == x = c
    | otherwise = pozinlist a (xs) (c+1)

pozimp lst = [y | y<-lst, mod (pozinlist y lst 0) 2 == 1 ]


-- newtype WriterS a = Writer { runWriter :: (a, String) } 


-- instance  Monad WriterS where
--   return va = Writer (va, "")
--   ma >>= k = let (va, log1) = runWriter ma
--                  (vb, log2) = runWriter (k va)
--              in  Writer (vb, log1 ++ log2)


-- instance  Applicative WriterS where
--   pure = return
--   mf <*> ma = do
--     f <- mf
--     a <- ma
--     return (f a)     

--subiectul3
newtype ReaderWriter env a = RW {getRW :: env-> (a,String)}

instance Monad (ReaderWriter env) where
    return va = RW (\_ -> (va, ""))
    ma >>= k = RW f
        where f env = let (va, str1) = getRW ma env
                          (vb, str2)  = getRW (k va) env
                    in (vb, str1 ++ str2)

instance Applicative (ReaderWriter env) where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor (ReaderWriter env) where              
  fmap :: (a -> b) -> ReaderWriter env a -> ReaderWriter env b
  fmap f ma = pure f <*> ma  

f1 :: Int -> ReaderWriter Int Int
f1 x = RW (\_ -> if x > 0 then (x, "Positive value!") else (x, "Negative value!"))

g1 :: Int -> ReaderWriter Int String
g1 y = RW (\_ -> (show y, "This is env value!"))


newtype Aduna env a = A{getA :: env -> (a, Int)}

instance Monad (Aduna env) where
    return a = A (\_ -> (a, 0))
    ma >>= k = A f
        where f env = let (va, ct1) = getA (ma) env
                          (vb, ct2) = getA (k va) env
                        in (vb, ct2 + ct1)

instance Applicative (Aduna env) where
  pure = return 
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor (Aduna env) where              
  fmap f ma = pure f <*> ma  

f2 :: Int -> Aduna Int Int
f2 x = A (\_ -> if x > 0 then (x, x) else (x, 0))

g2 :: Int -> Aduna Int Int
g2 x = A (\y -> if y > 0 then (x, y) else (x, 0))

test [] l = True
test (x:xs) l = (length x == l) && (test xs l) 

sumList x = [sum(y) | y<-x]

sumList1 :: [[Int]] -> [Int]
sumList1 x = do
    l <- x
    if length l /= 0 then
        return (sum l)
    else [0]

patrateImp x = [y*y | y<-x, mod y 2 == 1]

patrateImp1 x = do
    y <- x
    if mod y 2 == 1 then
        return (y*y)
    else [] 

fctpre x = if mod x 2 == 1 then 2*x else div x 2

pre12 x = map fctpre x

pre12DO x = do
    y <- x 
    if mod y 2 == 1 then
        return (2*y)
    else [(div y 2)]


--fct care ret patratele el de pe poz impare
form_list x = zip x [1..length x]

patratePozImp x = [ (fst y)*(fst y) | y <- form_list(x), mod(snd y) 2 == 1]

patratePozImp1 x = do
    y <- form_list x
    if mod (snd y) 2 == 1 then
        return ((fst y) * (fst y))
    else []