{-
- Usando os predicados not,and e or prontos de Haskell, implemente os predicados (funcoes) xor (or exclusivo),
- impl (implicacao A => B é equivalente a (not A or B)) e equiv (A <=> B é definido como A => B and B => A)
- Procure usar casamento de padroes e reutilizar as funcoes.
-}
xor False True = True
xor True False = True
xor _ _ = False


{-
Resposta alternativa
xor a b = or [ and [not a,b], and [a, not b] ]
impl a b = or [ not a, b ]
equiv a b = and [impl a b, impl b a]
-}

{-
A funcao square esta implementada e eleva ao quadrado um determinado numero
-}
square x = x*x

{-
- Implemente a funcao potencia, que retorna o resultado de x elevado a y 
-}
pow x 1 = x
pow x y = x* pow x (y-1)


{-
- Implemente a funcao fatorial que calcula o fatorial de um numero 
-}
fatorial 0 = 1
fatorial 1 = 1
fatorial x = x * fatorial (x-1)

{-alternative solution-}
fac 0 = 1
fac n = n * fac(n-1)

{-fast tail recursion-}
facAux 0 r = r
facAux n r = facAux (n-1) (r*n)
facTail n = facAux n 1

{-
- Determina se um numero eh primo ou nao. Preocupe-se apenas em resolver o problema.
- Nao precisa usar conhecimentos mais sofisticados da teoria dos numeros. Voce pode trabalhar com listas.
-}
isPrime 1 = False
isPrime x = notElem 0 ( map (mod x) (take (x-2) (iterate (1+)2)) )

isPrime' 1 = False
isPrime' x = notElem 0 (map (mod x) [2..(x-1)])

{-
- Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas. 
-}
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib x = fib (x-1)  + fib (x-2)


{-
fib tail rec-}
fibAux n result previous 
	| n == 0 = result 
	| otherwise = fibAux (n-1) (result+previous) result 
fibTail n 
	| n == 0 = 0 
	| otherwise = fibAux n 1 0

{-
- Calcula um MDC de dois numeros usando o algoritmo de Euclides. 
-}
mdc x y | y == 0 = x
        | otherwise = mdc y (mod x y)
{-
Outra forma:
mdc x 0 = x
mdc x y = mdc y (mod x y)
-}
{-
- Calcula um MMC de dois numeros. 
-}
mmc x y = div (x*y) (mdc x y) {-   É provado que: mmc(a,b)*mdc(a,b) = a*b    -}

{-
- Determina se dois numeros inteiros positivos sao co-primos. Dois numeros sao co-primos se 
- o mdc deles for igual a 1. Ex: coprimo 35 64 = True 
-}
coprimo x y | mdc x y == 1 = True
            | otherwise = False

coprimo' x y = (mdc x y) == 1

{-
- Calcula a conjectura de Goldbach, que diz que um numero par maior que 2 pode ser escrito como a soma de dois numeros primos. Ex: 28 = 5 + 23.
-}
goldbach x = [(a,b)| a <- (filter isPrime [2..x]), b <- (filter isPrime [2..x]), a+b==x]

--------------------------------------------------------------------------------------------
-- Adicional ---------------------------------------
addVectors a b = (fst a + fst b, snd a + snd b)
addVectors' (x1,x2) (y1,y2) = (x1 + y1, x2 + y2)


z `myOp` d  | or [and [not z, d], and [z, not d]] = GT
            | otherwise = EQ


a `myCompare` b | a > b = GT
                | a == b = EQ
                | otherwise = LT

initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

odd' = not . even
odd'' = \x -> (not . even)x

const x = \_ -> x -----------
