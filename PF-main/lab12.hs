elem1 :: (Foldable t, Eq a) => a -> t a -> Bool
elem1 x xs = foldr f False xs
    where f y acc = acc || x==y

null1 :: (Foldable t) => t a -> Bool
null1 xs = foldr (const (const False)) True xs

length1 :: (Foldable t) => t a -> Int
length1 xs = foldr (const(+1)) 0 xs

toList1 :: (Foldable t) => t a -> [a]
toList1 = foldr (:) []

fold1 :: (Foldable t, Monoid m) => t m -> m
fold1 xs = foldr mappend mempty xs

data Constant a b = Constant b
    deriving Show

instance Foldable (Constant a) where
    foldr f c (Constant a) = f a c

data Two a b = Two a b
    deriving Show

instance Foldable (Two a) where
    foldr f v (Two a b) = f b v

data Three a b c = Three a b c
    deriving Show

instance Foldable (Three a b) where
    foldr f v (Three a b c) = f c v

data Three' a b = Three' a b b
    deriving Show

instance Foldable (Three' a) where
    foldr f v (Three' a b c) = f b (f c v)


data Four' a b = Four' a b b b
    deriving Show

instance Foldable (Four' a) where
    foldr f v (Four' a b c d) = f b (f c (f d v))


data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) 
    deriving Show

instance Foldable (GoatLord) where
    foldr f v (NoGoat) = v
    foldr f v (OneGoat c) = f c v 
    foldr f v (MoreGoats a b c) = foldr f( foldr f(foldr f v a) b ) c 