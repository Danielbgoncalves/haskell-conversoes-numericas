-- | Esse modulo se propõem a fornecer a biblioteca com as funções que tornam o funncionamento da main viável
-- Luana Rodrigues e Daniel Borges 
module Biblioteca where
import Data.Char (toUpper)


-- |Função auxiliar que retorna o primeiro algarismo de um inteiro
primAlg :: Int -> Int
primAlg n = read [head (show n)] :: Int

-- | Função auxiliar que retorna a cauda de um inteiro após ser convertido para string
caudaBin :: Int -> String
caudaBin n = (tail (show n))

-- | Função que recebe um inteiro em binário e retorna um decimal, ela usa as funções auxiliares caudaBin e primAlg
funcao1 :: Int -> Int
funcao1 b
    | length (show b) == 1 = b
    | otherwise  = (primAlg b) * (2^length (caudaBin b)) + funcao1 (read((caudaBin b))::Int)



-- | Função que recebe um hexadecimal e retorna um decimal
funcao2 :: String -> Int
funcao2 [] = 0
funcao2 (x:xs)
     | x >= '0' && x <= '9' = ((fromEnum x)-48) * (16^(length xs)) + funcao2 xs
     | otherwise = ((fromEnum(toUpper x)) - 55) * (16^(length xs)) + funcao2 xs





-- | Função auxiliar que recebe uma lista de inteiros que representam o binário e o transforma para um inteiro 
listToInt :: [Int] -> Int
listToInt xs = read (concatMap show (reverse xs)) :: Int

-- | Função auxiliar que recbe inteiro e retorna uma lista os concatenando
intToList :: Int -> [Int]
intToList 0 = [0]
intToList 1 = [1]
intToList d = (a:intToList (div d 2))
    where
     a = d `mod` 2

-- | Função que recbe um decimal e retorna sua conversão para binário.
funcao3 :: Int -> Int
funcao3 d = listToInt (intToList d) :: Int





-- | Função que recebe um inteiro decimal e retorna um hexadecimal na forma de string
funcao4 :: Int -> String
funcao4 x
        | x >= 0 && x <= 9 = show x
        | x >= 10 && x <= 15 = [toEnum (x+55)]
        | otherwise = reverse ((a:funcao4 (div x 16)))
    where
    a =  if (x `mod` 16 >= 0 && x `mod`16 <= 9)
          then ( toEnum ((x `mod` 16)+48))
          else ( toEnum ((x `mod` 16)+55))





-- | Função que recebe um inteiro binário e o retorna como hexadeciaml na forma de string
funcao5 :: Int -> String
funcao5 b = funcao4(funcao1 b)




-- |Função que recebe um hexadecimal e retorna sua conversão para binário
funcao6 :: String -> Int
funcao6 xs = funcao3(funcao2 xs)





{- | Função auxiliar que recebe uma string que contenha um número com vírgula e retorna a quantidade de casas antes dela, caso não tenha,
retorna um negativo avisando a ausência de virgula (ou ponto, são interpretados como equivalentes) -}
achaVirgula :: String -> Int
achaVirgula (x:xs)
        | length xs == 0 = (-46)
        | x == ',' = 0
        | x == '.' = 0
        | otherwise = 1 + (achaVirgula xs)


{- | Função auxiliar que recebe um número vindo como string e o retorna uma dupla, a primeira parte com os algarismos antes da virgula e os 
posteriores na segunda parte -}
strToDup :: String -> (String, String)
strToDup (x:xs) = (a,b)
    where
     a = take (achaVirgula (x:xs)) (x:xs)
     b = drop ((achaVirgula (x:xs)) + 1 ) (x:xs)

{- | Função auxiliar que recebe a parte pós virgula de um binario e o converte para decimal equivalente (o retorno é inicia com 0. esse
fato é tratado na função que a recebe) -}
fracBinToDec ::  String -> Float
fracBinToDec [] = 0
fracBinToDec (m:ms)
  | length (show m) == 1 = fromIntegral ((fromEnum m)-48) * (2^^(-1))
  | otherwise =  fromIntegral((fromEnum  m)-48) * (2^^(-length (m:ms))) + (fracBinToDec ms)

{- | Função auxiliar que recebe uma dupla de strings e retorna uma string única (resultado da concatenação das duas partes do binario que 
foram convertidas para decimal pelas funções auxiliares) -}
concfBinToDec :: (String, String) -> String
concfBinToDec (js,m) = show (funcao1 (read js)) ++ "." ++  drop 2 (show (fracBinToDec (reverse m)))

-- | Função que recebe um Binário fracionário e retorna seu conversão para Decimal equivalente
funcao7 :: String -> String
funcao7 s = concfBinToDec (strToDup (semVirgula s))




-- | Função auxiliar que concatena as partes decimais (calculadas por auxiliares) para binário fracionário
concDecFracToBin :: (String,String) -> String
concDecFracToBin (av,dv) = show (funcao3 (read av)) ++ "." ++ (fracDecToBin (read("0." ++ dv)::Double))


-- | Função auxiliar que recebe a parte do decimal pós vírgula e transforma para binário
fracDecToBin :: Double -> String
fracDecToBin 0 = "0"
fracDecToBin d
       | length (show d) >= 33 = []
       | n >= 1 = '1' : fracDecToBin (n - 1.0)
       | otherwise = '0' : fracDecToBin n
    where
     n = d * 2.0

-- | Funcão que converter um número decimal com vírgula para binário equivalente usando as funções auxiliares acima
funcao8 :: String -> String
funcao8 s = concDecFracToBin (strToDup s)





-- | Função que recebe dois binários e os soma retornando em binário -}
funcao9 :: String -> String -> String
funcao9 a b = funcao8(show (c + d))
  where
  c = (read (funcao7 (semVirgula a)) :: Double)
  d = (read (funcao7 (semVirgula b)) :: Double)

-- | Função auxiliar que usa da 'achaVirgula', caso a string recebida não tenha virgula ou ponto ela adicionao ponto flutuante (.0) no final
semVirgula :: String -> String
semVirgula [] = error "Inválida"
semVirgula s
     |achaVirgula s < 0 = s ++ ".0" 
     |otherwise = s

-- | Função auxiliar que define a quantidade de espaços que devem ser vazios para a identação na imprimirResultadoDa9
defineQtsEspaços :: String -> String -> (Int, Int)
defineQtsEspaços n m
    | c >= d    = (0, c - d)
    | otherwise = (d - c, 0)
    where
     c = achaVirgula (semVirgula n)
     d = achaVirgula (semVirgula m)

-- | Função auxiliar que coloca os espaços necessários em uma string para a identação da imprimirResultadoDa9
poeEspaços :: (Int, Int) -> (String, String)
poeEspaços (a,b) = (c,d)
    where
     c = espaçosIndividuais a
     d = espaçosIndividuais b

-- | Função auxiliar que auxilia a 'poeEspaços' colocando individualemente os espaços em cada string
espaçosIndividuais :: Int -> String
espaçosIndividuais n
                   | n == 0 = " "
                   | otherwise = ' ':(espaçosIndividuais (n-1))

{- | Função que sintetiza as suas auxiliares para gerar strings que contenham a quantidade necessária de espaços para a identação do
resultado da imprimirResultadoDa9 -}
resolveEspaços :: String -> String ->(String, String)
resolveEspaços g h = poeEspaços (defineQtsEspaços g h)
