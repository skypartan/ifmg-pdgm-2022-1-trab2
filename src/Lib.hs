{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE OverloadedStrings #-}

module Lib
  ( main,
  )
where

import Data.List
import Numeric 
import Control.Applicative
import qualified Data.ByteString.Lazy as ByteString
import Data.Csv
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)

data Compra = Compra
  { dia :: !String,
    produto :: !String,
    loja :: !String,
    quantidade :: !Int,
    unidade :: !String,
    preco :: !Float,
    total :: !Float
  }
  deriving (Generic, Show, FromNamedRecord)


opcao1 vec maux = do
  print "OPCAO1"
  prod <- getLine
  
  let result = V.filter ( \compra -> produto compra == prod ) vec 
      res | length result > 0 = "O produto '" ++ produto (V.head result) ++ "' foi comprado\n"
          | otherwise = "O produto '" ++ prod ++ "' nao foi comprado\n"
  
  putStrLn res

  menu vec ((res ++ "\n"):maux)

calculaTotal (x:xs) = x + calculaTotal xs 
calculaTotal [] = 0

opcao2 vec maux = do
  print "OPCAO2"
  prod <- getLine
  
  let result = V.filter ( \compra -> produto compra == prod ) vec
      soQtd = fmap quantidade result
      unid | length result > 0 = " " ++ (unidade (V.head result)) ++ "s"
           | otherwise = " unidades"
      res = "Quantidade comprada do produto '" ++ prod ++ "': " ++ show (calculaTotal (V.toList soQtd)) ++ unid ++ "\n"

  putStrLn res

  menu vec ((res ++ "\n"):maux)


formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""

opcao3 vec maux = do
  print "OPCAO3"
  prod <- getLine
  
  let result = V.filter ( \compra -> produto compra == prod ) vec
      soQtd = fmap total result
      res = "Valor total comprado do produto '" ++ prod ++ "': R$ " ++ (formatFloatN (calculaTotal (V.toList soQtd)) 2) ++ "\n"

  putStrLn res

  menu vec ((res ++ "\n"):maux)


mProdutos (x:[]) aux =
                        (aux ++ x ++ "\n")
mProdutos (x:xs) aux = 
                        mProdutos xs (aux ++ x ++ "\n")
mostraProdutos (x:xs) d = 
                        mProdutos (x:xs) ("Produtos comprados em " ++ d ++ ":\n")
mostraProdutos [] d =
                  ("Nada foi comprado em " ++ d ++ "\n")

opcao4 vec maux = do
  print "OPCAO4"
  prod <- getLine
  
  let result = V.filter ( \compra -> dia compra == prod ) vec
      soProdutos = fmap produto result
      --MostraProdutos sem duplicatas (nub tira as duplicatas)
      res = mostraProdutos (nub (V.toList soProdutos)) prod
 
  putStrLn res
                      
  menu vec ((res ++ "\n"):maux)


opcao5 vec maux = do
  print "OPCAO5"
  prod <- getLine
  
  let result = V.filter ( \compra -> loja compra == prod ) vec
      soTotal = fmap total result
      res = "Quantidade comprada na loja '" ++ prod ++ "': R$ " ++ (formatFloatN (calculaTotal (V.toList soTotal)) 2) ++ "\n"

  putStrLn res

  menu vec ((res ++ "\n"):maux)


inverteL (x:xs) aux = inverteL xs (x:aux)
inverteL [] aux = aux

inverteLista (x:xs) = inverteL (x:xs) []
inverteLista [] = []

calcProdutos (x:xs) vec aux =
  let soX = V.filter (\compra -> produto compra == x) vec
      totaisdoX = fmap quantidade soX
  in calcProdutos xs vec ((calculaTotal (V.toList totaisdoX)):aux)

calcProdutos [] _ aux = inverteLista aux
 

pegaMaisComprados (x:xs) (y:ys) maior items 
  | y > maior = pegaMaisComprados xs ys y (x:[]) 
  | y == maior = pegaMaisComprados xs ys y (x:items)
  | otherwise = pegaMaisComprados xs ys maior items

pegaMaisComprados [] [] _ items = items

pProds [] aux = aux

pProds (x:[]) aux =
  (aux ++ x ++ "\n")

pProds (x:xs) aux =
  pProds xs (aux ++ x ++ "\n")

printProds (x:xs) =
  let prePrint | length (x:xs) == 0 = "Nao foi comprado nenhum produto ainda"
               | length (x:xs) == 1 = "O produto mais comprado foi: \n"
               | length (x:xs) > 1 = "Houve empate. Os produtos mais comprados foram: \n"
  in pProds (x:xs) prePrint


opcao6 vec maux = do
  print "OPCAO6"
  
  let produtosDistintos = (nub (V.toList (fmap produto vec)))
      qtdTotalPorProduto = calcProdutos produtosDistintos vec []
      res = (printProds $ pegaMaisComprados produtosDistintos qtdTotalPorProduto 0 []) 

  putStrLn res

  menu vec ((res ++ "\n "):maux)

inverteB :: [[a]] -> [[a]] -> [[a]]
inverteB (x:xs) aux = inverteB xs (x:aux)
inverteB [] aux = aux

inverteBuffer :: [[a]] -> [[a]]
inverteBuffer (x:xs) = inverteB (x:xs) [[]]
inverteBuffer [] = []

printRelat (x:xs) aux = printRelat xs (aux ++ x)
printRelat [] aux = putStrLn aux

opcao7 vec maux = do
  print "OPCAO7"
  
  let relatorio = inverteBuffer maux

  -- aqui que vai escrever num arquivo
  printRelat relatorio ""

  menu vec []

--menu :: IO ()
menu vec maux = do
  print "Digite uma opcao: "

  op <- getLine
  let action
        | op == "sair" = print "Finalizado com sucesso"
        | op == "1" = opcao1 vec maux
        | op == "2" = opcao2 vec maux
        | op == "3" = opcao3 vec maux
        | op == "4" = opcao4 vec maux
        | op == "5" = opcao5 vec maux
        | op == "6" = opcao6 vec maux
        | op == "7" = opcao7 vec maux
        | otherwise = menu vec maux 
  
  action


main :: IO ()
main = do
  bytes <- ByteString.readFile "./input.csv"

  case decodeByName bytes of
    Left string -> fail string
    Right (_header, rows) -> do
      print (rows :: Vector Compra)
      print "\n\n"
      menu rows []

-- pesquisar como escrever num arquivo texto e como pegar parametro da linha de comando