--Exercício 1: A
gabriel :: Bool -> Bool -> Bool
gabriel True True = True
gabriel True False = True
gabriel False True = True
gabriel False False = False

gabriel2 :: Bool -> Bool -> Bool
gabriel2 False False = False
gabriel2 True _ = True

gabriel3 :: Bool -> Bool -> Bool
gabriel3 False b = b
gabriel3 True _ = True

--B
gabriel4 :: Bool -> Bool -> Bool
gabriel4 a b =
  if (a == False && b == False)
    then False
    else True

gabriel5 :: Bool -> Bool -> Bool
gabriel5 a b =
  if (a /= b)
    then True
    else
      if (a == b && b == False)
        then False
        else True

--Exercício 2
quad :: Float -> Float
quad x = x * x

dist :: (Float, Float) -> (Float, Float) -> Float
dist (x1, y1) (x2, y2) = sqrt (quad (x2 - x1) + quad (y2 - y1))

--Exercício 3
fat :: Int -> Int
fat 0 = 1
fat n = n * fat (n -1)

fatorialIf :: Int -> Int
fatorialIf x = if(x==0) then 1
            else (x*fatorialIf (x-1))

--Exercício 4
fibo :: Int -> Int
fibo n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = fibo (n -2) + fibo (n -1)

--Exercício 5
n_tri :: Int -> Int
n_tri n
  | n == 0 = 0
  | n == 1 = 1
  | n == 2 = 3
  | n == 3 = 6
  | otherwise = n_tri (n -3) + n_tri (n -2) + n_tri (n -1)

  --Exercício 6
potencia2 :: Int -> Int
potencia2 expo
  | expo == 0 = 1
  | otherwise = (2 * potencia2 (expo -1))

  --Exercício 7
--A
prodIntervalo :: Int -> Int -> Int
prodIntervalo m n
  | m == n = n
  | otherwise = n * prodIntervalo m (n -1)
--B
fiboProdIntervalo :: Int -> Int
fiboProdIntervalo  n
  | 1 == n = n
  | otherwise = n * fiboProdIntervalo  (n -1)

  --Exercício 8
resto_div :: Int -> Int -> Int
resto_div dividendo divisor =
  if dividendo == 0 || divisor == 1
    then 0
    else
      if dividendo < divisor
        then dividendo
        else resto_div (dividendo - divisor) divisor

div_inteira :: Int -> Int -> Int
div_inteira dividendo divisor =
  if (dividendo < divisor)
    then 0
    else (div_inteira (dividendo - divisor) divisor) + 1

    --Exercício 9
-- guardas:
mdc1 :: (Int, Int) -> Int
mdc1 (m, n)
  | n == 0 = m
  | otherwise = mdc1 (n, (mod m n))

--Casamento de Padrões
mdc2 :: (Int, Int) -> Int
mdc2 (m, 0) = m
mdc2 (m, n) = mdc2 (n, (mod m n))

--Exercício 10
-- Guardas
binomial1 :: (Int, Int) -> Int
binomial1 (n, k)
  | k == 0 = 1
  | k == n = 1
  | otherwise = binomial1 (n -1, k) + binomial1 (n -1, k -1)

--Casamento de Padrões
binomial2 :: (Int, Int) -> Int
binomial2 (n, 0) = 1
binomial2 (n, k) =
  if (k == n)
    then 1
    else binomial2 (n -1, k) + binomial2 (n -1, k -1)


--Exercício 11
passo :: (Int, Int) -> (Int, Int)
passo (x, y) = (y, x + y)

auxFibo :: Int -> (Int,Int)
auxFibo 1 = (1, 1)
auxFibo n = passo (auxFibo (n-1))

fibo2 :: Int -> Int
fibo2 n = do
    let (x, y) = auxFibo n
    x









