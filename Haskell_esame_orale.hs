import Data.Char
import Data.List
--Definizione di un volontario
data Volontario = 
   VS {nome::String, cognome::String, eta::Int}
  |CS {nome::String, cognome::String, num::String, eta::Int}

--Definisco l'istanza di Eq per stabilire se i volontari sono gli stessi
instance Eq Volontario where
  (==) (VS n1 c1 e1) (VS n2 c2 e2) = (n1==n2) && (c1==c2) && (e1==e2)
  (==) (CS n1 c1 t1 e1) (CS n2 c2 t2 e2) = (t1==t2)
  (==) (VS n1 c1 e1) (CS n2 c2 t2 e2) = False
  (==) (CS n2 c2 t2 e2) (VS n1 c1 e1) = False

--Creo qualche volontario per testare le funzioni definite
v1 = VS "Giorgia" "Bressanelli" 25
v2 = VS "Gabriele" "Bressanelli" 24
v3 = CS "Mariangela" "Tedoldi" "4444" 60
v4 = CS "Pietro" "Bressanelli" "5555" 55

--Creo la lista
volontari = [v1,v2,v3,v4]

--Funzione per visualizzare i dati in maiuscolo
visualizzaMaiuscolo :: [Char] -> [Char]
visualizzaMaiuscolo dato = map toUpper dato

--Funzione tipo toString
--Eventualmente esiste la Typeclass Show, che permette di visualizzare anche il tipo di record
toStringVolontario :: Volontario -> [Char]
toStringVolontario vol = "- " 
  ++ visualizzaMaiuscolo (nome vol) 
  ++ " " 
  ++ visualizzaMaiuscolo (cognome vol)

--Stampa di una lista
toStringTutti :: [Volontario] -> [Char]
toStringTutti [] = []
toStringTutti (x:xs) = toStringVolontario x ++ " - " ++ toStringTutti xs

--Controllo sul singolo volontario se è più vecchio di una data eta
olderThan :: Volontario -> Int -> [Char]
olderThan vol 0 = toStringVolontario vol
olderThan vol e
  | (eta vol) > e = toStringVolontario vol
  | otherwise     = []

--controllo su tutti -> posso farlo con list comprehension?
allOlderThan :: [Volontario] -> Int -> [Char]
allOlderThan [] _ = []
allOlderThan xs 0 = toStringTutti xs
allOlderThan (x:xs) et = (olderThan x et) ++ " - " ++ (allOlderThan xs et)

main =  do
  print(toStringTutti volontari)
  print(toStringVolontario v2)
  print(v1==v2)
  print(v4==v4)
  print(allOlderThan volontari 40)
  print(toStringTutti (reverse volontari))
  print(length volontari)
  
  