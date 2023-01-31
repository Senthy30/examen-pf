{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
    deriving Show

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

-- cu init.tail.show sterg primu si ultimu caracter (adica parantezele patrate si le adaug pe cele rotunde apoi)
instance Show Punct where
    show (Pt x) = "(" ++ (init . tail . show) x ++ ")"
a = Pt [1,2,3]

instance ToFromArb Punct where
    toArb (Pt[]) = Vid
    toArb (Pt (x:xs)) = N (F x) (toArb(Pt xs))
    fromArb Vid = Pt []
    fromArb (F x) = Pt[x]
    fromArb (N x y) = let Pt p1 = fromArb(x)
                          Pt p2 = fromArb(y)
                    in Pt(p1 ++ p2)

data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
    perimeter :: (Floating a) => g a -> a
    area :: (Floating a) => g a -> a

instance GeoOps Geo where
    perimeter (Square b) = 4*b
    perimeter (Rectangle a b) = 2*a + 2 * b
    perimeter (Circle a) = 2*pi*a

    area (Square b) = b*b
    area (Rectangle a b) = a*b
    area (Circle a) = pi*a*a

instance (Floating a, Eq a) => Eq (Geo a) where
    (Square b1) == (Square b2) = (perimeter (Square b1)) == (perimeter (Square b2))
    (Square b1) == (Rectangle a1 a2) = (perimeter (Square b1)) == (perimeter (Rectangle a1 a2))
    (Square b1) == (Circle b2) = (perimeter (Square b1)) == (perimeter (Circle b2))
    
    (Rectangle a1 b1) == (Rectangle a2 b2) = (perimeter (Rectangle a1 b1)) == (perimeter (Rectangle a2 b2))
    (Rectangle a1 b1) == (Circle b2) = (perimeter (Rectangle a1 b1)) == (perimeter (Circle b2))
    (Rectangle a1 b1) == (Square b2) = (perimeter (Rectangle a1 b1)) == (perimeter (Square b2))
    
    (Circle r1) == (Circle r2) = (perimeter (Circle r1)) == (perimeter (Circle r2))
    (Circle r1) == (Rectangle a1 a2) = (perimeter (Circle r1)) == (perimeter (Rectangle a1 a2))
    (Circle r1) == (Square r2) = (perimeter (Circle r1)) == (perimeter (Square r2))
    
    _ == _ = False