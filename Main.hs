-- |Programa que faz diversas convesões de bases numéricas
-- Luana Rodrigues e Daniel Borges

module Main (main) where

import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))
import Biblioteca
--import Text.Printf (printf)

-- | Função que  mostra uma mensagem que recolhe uma variável
prompt :: String -> IO(String)
prompt msg = do putStr msg
                getLine

-- | função que mostra as opções de escolha do usuário 
exibirOpções :: [String] -> IO ()
exibirOpções opções = do
  putStrLn ""
  putStrLn "****************************************************"
  putStrLn "*                C o n v e r s o r                 *"
  putStrLn "****************************************************"
  putStrLn ""
  putStrLn "Opções:"
  putStrLn ""
  putStrLn (unlines opções)
  putStrLn ""

{- |Função que recebe a opção escolhida pelo usuário e chama a função
 equilente -}
choosePrompt :: Int -> IO(String, String)
choosePrompt opção
        | opção == 1 = do 
                        n <- prompt strF1
                        return(n,"")
        | opção == 2 = do 
                        n <- prompt strF2
                        return(n,"")
        | opção == 3 = do 
                        n <- prompt strF3
                        return(n,"")
        | opção == 4 = do 
                        n <- prompt strF4
                        return(n,"")
        | opção == 5 = do 
                        n <- prompt strF5
                        return(n,"")
        | opção == 6 = do 
                        n <- prompt strF6
                        return(n,"")
        | opção == 7 = do 
                        n <- prompt strF7
                        return(n,"")
        | opção == 8 = do 
                        n <- prompt strF8
                        return(n,"")
        | opção == 9 = do n <- prompt strF9
                          m <- prompt strF10
                          return(n,m)
        | opção == 0 = return ("","")

     where
      strF1 = "Digite um binário: "
      strF2 = "Digite um hexadecimal: "
      strF3 = "Digite um natural: "
      strF4 = "Digite um natural: "
      strF5 = "Digite um binário: "
      strF6 = "Digite um hexadecimal: "
      strF7 = "Digite um binário fracionário: "
      strF8 = "Digite um real: "
      strF9 = "Digite o primeiro binário: "
      strF10 = "Digite o segundo binário: "


-- | Função que lê a opção de função escolhida do usuário e a valida
lerOpção :: Int -> Int -> IO Int
lerOpção mínima máxima = do putStr "Digite uma opção: "
                            opção <- readLn
                            valida opção
  where
    valida opção | mínima <= opção && opção <= máxima = return opção
                 | otherwise = do putStrLn "Opção inválida!"
                                  putStrLn ""
                                  putStr "Digite uma opção: "
                                  opção <- readLn
                                  valida opção

-- | Função que imprime o resultado das funções de 1 a 8
imprimirResultado :: Int -> String -> String -> IO ()
imprimirResultado opção n res = do
     putStrLn ""
     putStrLn "--------------------------------------------------"
     case opção of
        1 -> putStrLn strFuncao1
        2 -> putStrLn strFuncao2
        3 -> putStrLn strFuncao3
        4 -> putStrLn strFuncao4
        5 -> putStrLn strFuncao5
        6 -> putStrLn strFuncao6
        7 -> putStrLn strFuncao7
        8 -> putStrLn strFuncao8
        0 -> putStrLn "Fim"
     putStrLn "--------------------------------------------------"
  where
     strFuncao1 = concat [ n, "_(2) = ",  res, "_(10)"]
     strFuncao2 = concat [ n, "_(16) = ", res, "_(10)"]
     strFuncao3 = concat [ n, "_(2) = ",  res, "_(2)"]
     strFuncao4 = concat [ n, "_(10) = ", res, "_(16)"]
     strFuncao5 = concat [ n, "_(2) = ",  res, "_(16)"]
     strFuncao6 = concat [ n, "_(16) = ", res, "_(2)"]
     strFuncao7 = concat [ n, "_(2) = ",  res, "_(10)"]
     strFuncao8 = concat [ n, "_(10) = ", res, "_(2)"]


-- | Função que imprime o resultado da função 9
imprimirResultadoDa9 :: String -> String -> String -> IO()
imprimirResultadoDa9 n m res = do putStrLn ""
                                  putStrLn "--------------------------------------------------"
                                  linha1
                                  linha2
                                  putStrLn "------------                         -------------"
                                  linha4
                                  putStrLn "--------------------------------------------------"
   where
    (a,b) = (resolveEspaços n m)
    (c,d) = resolveEspaços (funcao7 n) (funcao7 m)
    z = espaçosIndividuais (42 - (length a) - (length n) - (length (funcao7 n)))
    w = espaçosIndividuais (41 - (length b) - (length m) - (length (funcao7 m)))
    y = espaçosIndividuais (38 - (length res) - (length (funcao7 res)))

    linha1 = putStrLn (concat ["  ",a, n,z,(funcao7 n)])
    linha2 = putStrLn (concat ["+ ",b,m,w,"+ ",(funcao7 m)])
    linha4 = putStrLn (concat ["   ", res,"_(2)",y, (funcao7 res),"_(10)"])
    --linha1 = putStrLn $ printf "  %s%40s" n (funcao7 n)
    --linha2 = putStrLn $ printf "+ %s%39s" m (funcao7 m)
    --linha4 = putStrLn $ printf "   %s_(2)%33s_(10)" res (funcao7 res)

-- | Função que imprime o menu e pega a opção escolhida pelo usuário
menu :: IO Int
menu = do exibirOpções texto
          lerOpção opMín opMáx
  where
    texto = [ "1 - converter de binário para decimal",
              "2 - converter de hexadecimal para decimal",
              "3 - converter de decimal para binário",
              "4 - converter de decimal para hexadecimal",
              "5 - converter de binário para hexadecimal",
              "6 - converter de hexadecimal para binário",
              "7 - converter de fração binária para decimal",
              "8 - converter de fração decimal para binário",
              "9 - somar dois binários sem sinal ",
              "0 - sair" ]
    opMín = 0
    opMáx = 9

-- | Função que pergunta ao usuário se ele quer fazer novamente a conversão anterior
repetir :: Int -> IO()
repetir opção = do
           case opção of 
             1 -> do deNovo <- prompt "Deseja converter outro binário (s/n)? "
                     if deNovo == "s" then do putStrLn ""
                                              (n,m) <- choosePrompt opção
                                              imprimirResultado opção n (show((funcao1 ((read n) :: Int)))) 
                                              repetir 1
                                      else laçoMenu
             2 -> do deNovo <- prompt "Deseja converter outro hexadecimal (s/n)? "
                     if deNovo == "s" then do putStrLn ""
                                              (n,m) <- choosePrompt opção
                                              imprimirResultado opção n (show((funcao2 n)))
                                              repetir 2
                                      else laçoMenu
             3 -> do deNovo <- prompt "Deseja converter outro natural (s/n)? "
                     if deNovo == "s" then do putStrLn ""
                                              (n,m) <- choosePrompt opção
                                              imprimirResultado opção n (show((funcao3 ((read  n) :: Int))))
                                              repetir 3
                                      else laçoMenu
             4 -> do deNovo <- prompt "Deseja converter outro natural (s/n)? "
                     if deNovo == "s" then do putStrLn ""
                                              (n,m) <- choosePrompt opção
                                              imprimirResultado opção n (funcao4 ((read  n) :: Int))
                                              repetir 4
                                      else laçoMenu
             5 -> do deNovo <- prompt "Deseja converter outro binário(s/n)? "
                     if deNovo == "s" then do putStrLn ""
                                              (n,m) <- choosePrompt opção
                                              imprimirResultado opção n (funcao5 ((read n) :: Int)) 
                                              repetir 5 
                                              else laçoMenu
             6 -> do deNovo <- prompt "Deseja converter outro hexadecimal (s/n)? "
                     if deNovo == "s" then do putStrLn ""
                                              (n,m) <- choosePrompt opção
                                              imprimirResultado opção n (show((funcao6 n)))
                                              repetir 6
                                      else laçoMenu
             7 -> do deNovo <- prompt "Deseja converter outro binário fracionário (s/n)? "
                     if deNovo == "s" then do putStrLn ""
                                              (n,m) <- choosePrompt opção
                                              imprimirResultado opção n (funcao7 n)
                                              repetir 7
                                      else laçoMenu
             8 -> do deNovo <- prompt "Deseja converter outro real (s/n)? "
                     if deNovo == "s" then do putStrLn ""
                                              (n,m) <- choosePrompt opção
                                              imprimirResultado opção n (funcao8 n)
                                              repetir 8
                                      else laçoMenu
             9 -> do deNovo <- prompt "Deseja somar outrso binários (s/n)? "
                     if deNovo == "s" then do putStrLn ""
                                              (n,m) <- choosePrompt opção
                                              imprimirResultadoDa9 n m (funcao9 n m)
                                              repetir 9
                                      else laçoMenu

-- | Função que faz um laço que repete o menu
laçoMenu :: IO ()
laçoMenu = do
   opção <- menu
   (n, m) <- choosePrompt opção
   case opção of
     1 -> do imprimirResultado opção n (show((funcao1 ((read n) :: Int))))
             repetir 1
     2 -> do imprimirResultado opção n (show((funcao2 n)))
             repetir 2
     3 -> do imprimirResultado opção n (show((funcao3 ((read n) :: Int))))
             repetir 3
     4 -> do imprimirResultado opção n (funcao4 ((read n) :: Int))
             repetir 4
     5 -> do imprimirResultado opção n (funcao5 ((read n) :: Int))
             repetir 5
     6 -> do imprimirResultado opção n (show((funcao6 n)))
             repetir 6
     7 -> do imprimirResultado opção n (funcao7 n)
             repetir 7
     8 -> do imprimirResultado opção n (funcao8 n)
             repetir 8
     9 -> do imprimirResultadoDa9 n m (funcao9 n m)
             repetir 9
     0 -> do imprimirResultado opção n "0"
             return ()

-- | Função main, a primiera a ser executada e coordena o programa
main :: IO ()
main = do hSetBuffering stdout NoBuffering
          laçoMenu
