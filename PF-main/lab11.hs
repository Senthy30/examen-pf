data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

instance Functor List  where
    fmap f Nil = Nil
    fmap f (Cons a b) = Cons(f a) (fmap f b)

instance Applicative List where
    pure x = Cons x Nil 
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons a la) <*> xs = append  (fmap a xs) (la <*> xs)

append :: List a -> List a -> List a
append Nil x = x
append (Cons x xs) ys = Cons x (append xs ys)

f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)
test1 = (f <*> v) == Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))


data Cow = Cow {
    name :: String
    , age :: Int
    , weight :: Int
    } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty a = if length a == 0 then Nothing
    else Just a

noNegative :: Int -> Maybe Int
noNegative a = if a > 0 then Just a
    else Nothing

-- cowFromString :: String -> Int -> Int -> Maybe Cow
-- cowFromString n a w = if (noEmpty n /= Nothing) && (noNegative w /= Nothing) && (noNegative w /= Nothing) then Just (Cow n a w)
--     else Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString n a w = Cow <$> noEmpty n <*> noNegative a <*> noNegative w



newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)
data Person = Person Name Address
    deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength a str = if length str < a then Just str
    else Nothing

mkName :: String -> Maybe Name
mkName a = if validateLength 25 a /= Nothing then Just (Name a)
    else Nothing

mkAddress :: String -> Maybe Address
mkAddress b = if validateLength 100 b /= Nothing then Just (Address b)
    else Nothing


test32 = mkName "Gigel" == Just (Name "Gigel")
test33 = mkAddress "Str Academiei" == Just (Address "Str Academiei")


-- mkPerson :: String -> String -> Maybe Person
-- mkPerson a b = if mkAddress b /= Nothing && mkName a /= Nothing then Just (Person (Name a)  (Address b)) 
--     else Nothing

mkPerson :: String -> String -> Maybe Person
mkPerson a b = Person <$> mkName a <*> mkAddress b
test34 = mkPerson "Gigel" "Str Academiei" == Just (Person (Name "Gigel") (Address "Str Academiei"))