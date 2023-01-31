{- Monada Maybe este definita in GHC.Base 

instance Monad Maybe where
  return = Just
  Just va  >>= k   = k va
  Nothing >>= _   = Nothing


instance Applicative Maybe where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor Maybe where              
  fmap f ma = pure f <*> ma   
-}

pos :: Int -> Bool
pos  x = if (x>=0) then True else False

fct :: Maybe Int ->  Maybe Bool
-- fct  mx =  mx  >>= (\x -> Just (pos x))
fct mx = do
    x <- mx
    return (pos x)


addM :: Maybe Int -> Maybe Int -> Maybe Int
addM mx my = do
    x <- mx
    y <- my
    return (x + y)

-- cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))
cartesian_product xs ys = do
    x <- xs
    y <- ys
    return (x, y)


-- prod f xs ys = [f x y | x <- xs, y<-ys]
-- prod :: Monad m => (m t1 -> m t2 -> m b) -> [m t2] -> [m t1] -> [m b]
prod f xs ys = do
    x <- xs
    y <- ys
    return f x y

myGetLine :: IO String
-- myGetLine = getChar >>= \x ->
--       if x == '\n' then
--           return []
--       else
--           myGetLine >>= \xs -> return (x:xs)
myGetLine = do
    x <- getChar
    if x == '\n' then
        return []
    else do
        xs <-myGetLine
        return (x:xs)


prelNo noin =  sqrt noin

ioNumber = do
     noin  <- readLn :: IO Float
     putStrLn $ "Intrare\n" ++ (show noin)
     let  noout = prelNo noin
     putStrLn $ "Iesire"
     print noout

ioNumber1 = 
    readLn >>= \noin -> 
    putStrLn ("Intrare\n" ++ (show noin)) >>
    let noout = prelNo noin in 
    putStrLn "Iesire" >>
    print noout
    
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
logIncrement x = do
    tell ( " increment : " ++ show x ++ " \n" )
    return ( x + 1 )

logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x n = do
    y <- logIncrement x
    if n > 1 then
        logIncrementN y (n - 1)
    else return y
        
   


data Person = Person { name :: String, age :: Int }

showPersonN :: Person -> String
showPersonN (Person n a) = ("NAME: " ++ n) 


showPersonA :: Person -> String
showPersonA (Person n a) = ("VARSTA: " ++ show a) 


showPerson :: Person -> String
showPerson (Person n a) = ("NAME: " ++ n ++ ", VARSTA: " ++ show a)

{-
showPerson $ Person "ada" 20
"(NAME: ada, AGE: 20)"
-}


newtype Reader env a = Reader { runReader :: env -> a }


instance Monad (Reader env) where
  return x = Reader (\_ -> x)
  ma >>= k = Reader f
    where f env = let a = runReader ma env
                  in  runReader (k a) env



instance Applicative (Reader env) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance Functor (Reader env) where              
  fmap f ma = pure f <*> ma    

mshowPersonN ::  Reader Person String
mshowPersonN = do
    person <- Reader $ \env -> env
    return $ "Name: " ++ name person

mshowPersonA ::  Reader Person String
mshowPersonA = do
    person <- Reader $ \env -> env
    return $ "Age: " ++ show (age person)

mshowPerson ::  Reader Person String
mshowPerson = do
    person <- Reader $ \env -> env
    return $ "Name: " ++ name person ++ ", Age: " ++ show (age person)

--runReader mshowPersonN  $ Person "ada" 20
--"NAME:ada"
