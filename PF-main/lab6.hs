data Fruct = Mar String Bool | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
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
ePortocalaDeSicilia(Portocala "Valencia" _) = False
ePortocalaDeSicilia(Portocala _ _) = True
ePortocalaDeSicilia _ = False

nrFeliePortocala :: Fruct -> Int
nrFeliePortocala (Portocala _ a) = a
nrFeliePortocala _ = 0

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia [] = 0
nrFeliiSicilia (x:xs) = 
    if ePortocalaDeSicilia(x) then
        nrFeliePortocala(x) + nrFeliiSicilia(xs)
    else
        nrFeliiSicilia(xs)

marViermi :: Fruct -> Bool
marViermi (Mar _ True) = True
marViermi _ = False

nrMereViermi :: [Fruct] -> Int
nrMereViermi [] = 0
nrMereViermi (x:xs) =
    if marViermi(x) then
        1+nrMereViermi(xs)
    else
        nrMereViermi(xs)



type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow"
vorbeste (Caine _ _) = "woof"

rasa :: Animal -> Maybe String
rasa (Pisica _) = Nothing
rasa (Caine _ r) = Just r

a = Pisica "a"
b = Caine "nume" "rasa"

data Linie = L [Int]
    deriving Show
data Matrice = M [Linie]
    deriving Show

ma = M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]

verifica_linie :: [Int] -> Int -> Bool
verifica_linie l n = if foldr (+) 0 l == n then True
    else False


verifica :: Matrice -> Int -> Bool
verifica (M m) n = foldr ( \(L line) result -> result && (verifica_linie line n) ) True m


verificaLinie :: Linie -> Int -> Bool
verificaLinie (L l) a = if length l == a then length( filter (>0) l) == a    
    else True 

doarPozN :: Matrice -> Int -> Bool
doarPozN (M m) a = foldr (\(L line) result -> result && verificaLinie(L line) a ) True m

