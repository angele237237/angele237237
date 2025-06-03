---EXERCICE 1

---Exercice 1.1 Fonction pgcd de 2 entiers
pgcd::(Int, Int)->Int
pgcd(a, b)
    |a == b = a
    |a > b = pgcd(a-b, b)
    |a < b = pgcd (a, b-a)

---Exercice 1.2 Fonction ppcm de 2 entiers
ppcm :: (Int, Int)-> Int
ppcm(a, b) = a * b `div` pgcd(a, b)

---Exercice 1.3 Somme impair
sommeImpair :: Int->Int
sommeImpair n = if n == 1 then 1 else (sommeImpair (n-1)+(2*(n-1)+1) )

---Exercice 1.4 Division entiere
divisionEntiere :: Int->Int-> (Int, Int)
divisionEntiere a b = (a`div`b, a`mod`b)

---Exercice 1.5 Minimum et maximum de deux nombres
myMin :: (Int, Int) -> Int
myMin (a, b)= if a<= b then a else b

myMax:: (Int, Int)-> Int
myMax(a, b) = if a>=b then a else b

---Exercice 1.6 Minimum et maximum de quatres nombres
myMin4 :: Int-> Int-> Int-> Int-> Int
myMin4 a b c d = myMin ((myMin (a, b )), (myMin (c, d)))

myMax4 :: Int-> Int-> Int-> Int-> Int
myMax4 a b c d = myMax ((myMax (a, b)), (myMax(c, d)))

---Exercice 1.7 Fonction borner dans
bornerDans :: Int-> Int-> Int-> Int
bornerDans a b c 
               |c < bornemin= bornemin
               |c > bornemax= bornemax
               |otherwise = c
                            where
                                bornemin= myMin (a, b)
                                bornemax= myMax (a, b) 

---Exercice 1.8 Somme des chiffres
sommeChiffre :: Int-> Int
sommeChiffre n
              |n<10 = n  
              |n>=10 = (n `mod` 10) + sommeChiffre(div n 10)


---Exercice 1.9 Nombres premiers
diviseur::Int-> [Int]
diviseur n = [i |i <- [1..n], n `mod` i == 0]
premier:: Int-> Bool
premier n
      |length(diviseur n) == 2 = True
      |length(diviseur n) > 2 = False
      |length(diviseur n) < 2 = False


--- Exercice 1.10 Calcule de la distance [AB]
type Point = (Double, Double)
distance :: Point -> Point-> Double
distance (x1, y1) (x2, y2) = sqrt((x2-x1)^2 + (y2-y1)^2)

---Exercice 1.11 resolution d'une suite
u:: Int->Int
u n
 |n== 0 = -2
 |n > 0 = 3 + 4 * u(n-1)

---Exercice 1.12 Fonction effectuant des sommes
---Cas 1
i::Int-> Int
i n = if n == 0 then 0 else i(n-1) + n

---Cas 2
j::Int-> Int
j n = if n == 0 then 0 else j(n-1) + n ^ 2

---Cas 3
sn::Float->Float
sn k 
   |k== 1 = 1
   |k > 1 = sn(k-1) + (1/k)

---Cas 4
g:: Float-> Float
g n = if n == 1 then 1 else g(n-1) + ( (-1**(n-1)) / n)

---Cas 5
h:: Float-> Float
h n = if n == 1 then 1 else h(n-1) + (1 / n**2)



---Exercice 2
data Resultat = Singleton Float | Couple Float Float deriving ( Show)
snddegres:: Float-> Float ->Float -> Resultat
snddegres a b c 
               |delta < 0 = error "pas de solution"
               |delta == 0 = Singleton (- b / (2 * a))
               |delta > 0 = Couple x1 x2
                        where
                            delta = sqrt ((b ^ 2) - 4 * a * c)
                            x1 = ((- b - delta) / (2 * a))
                            x2 = ((- b + delta) / (2 * a))



---Exercice 3 
data Jour = Lundi | Mardi | Mercredi | Jeudi | Vendredi | Samedi | Dimanche    deriving (Show)
weekend:: Jour-> Bool
weekend Samedi   = True
weekend Dimanche = True
weekend _        = False

numeroJour:: Jour-> Int
numeroJour Lundi    = 1
numeroJour Mardi    = 2
numeroJour Mercredi = 3
numeroJour Jeudi    = 4
numeroJour Vendredi = 5
numeroJour Samedi   = 6
numeroJour Dimanche = 7



---Exercice 4 Produit scalaire de deux vecteurs
type Couple =(Integer, Integer)
produitScalaire:: Couple-> Couple-> Integer
produitScalaire (x1, y1) (x2, y2) = x1 * x2 + y1 * y2