import Prelude hiding (Functor, fmap)

class Functor f where
  fmap :: (a -> b) -> f a -> f b
newtype Identity a = Identity a

instance Functor Identity where
    fmap f (Identity a) = (Identity (f a))

data Pair a = Pair a a

---a

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

data Constant a b = Constant b

instance Functor (Constant a) where
    fmap f (Constant b) = Constant (f b)

data Two a b = Two a b

instance Functor (Two a) where
    fmap f (Two a b) = Two (a) (f b) 

data Three a b c = Three a b c

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

data Four a b c d = Four a b c d

instance Functor (Four a b c) where
    fmap f (Four a b c d) =  Four a b c (f d) 

data Four'' a b = Four'' a a a b

instance Functor (Four'' a) where
    fmap f (Four'' a b c d) = Four'' a b c (f d)

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
    fmap f (Bloor a) = Bloor (f a)
    fmap f Finance = Finance
    fmap f (Desk a) = Desk (a)

---b
data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
    fmap g (LiftItOut f) = LiftItOut(fmap g f)


data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor ( Parappa f g ) where
    fmap g (DaWrappa x y) = DaWrappa (fmap g x) (fmap g y)


data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
    fmap g (IgnoringSomething f h) = IgnoringSomething(f) (fmap g h)

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor f) => Functor (Notorious f g h) where
    fmap g (Notorious a b c) = Notorious a b (fmap g c)


data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor(GoatLord) where    
    fmap f NoGoat = NoGoat
    fmap f (OneGoat x) = OneGoat(f x)
    fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor(TalkToMe) where
    fmap f Halt = Halt
    fmap f (Print a b) = Print (a) (f b)
    fmap f (Read x) = Read (f.x)