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

  let comprado | length result > 0 = "O produto " ++ produto (V.head result) ++ " foi comprado"
               | otherwise = "O produto nao foi comprado"
  
  print comprado

  menu vec maux

calculaTotal (x:xs) = x + calculaTotal xs 
calculaTotal [] = 0

opcao2 vec maux = do
  print "OPCAO2"
  prod <- getLine
  
  let result = V.filter ( \compra -> produto compra == prod ) vec
  let soQtd = fmap quantidade result

  let unid | length result > 0 = " " ++ (unidade (V.head result)) ++ "s"
           | otherwise = " unidades"

  print ("Quantidade comprada do produto: " ++ show (calculaTotal (V.toList soQtd)) ++ unid)
  menu vec maux


formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""

opcao3 vec maux = do
  print "OPCAO3"
  prod <- getLine
  
  let result = V.filter ( \compra -> produto compra == prod ) vec
  let soQtd = fmap total result


  print ("Valor total comprado do produto: R$ " ++ (formatFloatN (calculaTotal (V.toList soQtd)) 2))
  menu vec maux


mProdutos (x:[]) = do
                        print x
mProdutos (x:xs) = do
                        print x
                        mProdutos xs
mostraProdutos (x:xs) d = do
                        print ("Produtos comprados em " ++ d ++ ":")
                        print ""
                        mProdutos (x:xs)
mostraProdutos [] d = do
                  print ("Nada foi comprado em " ++ d)

opcao4 vec maux = do
  print "OPCAO4"
  prod <- getLine
  
  let result = V.filter ( \compra -> dia compra == prod ) vec
  let soProdutos = fmap produto result
 
  --MostraProdutos sem duplicatas
  mostraProdutos (nub (V.toList soProdutos)) prod
  --mostraProdutos (V.toList soProdutos) prod
                      
  menu vec maux


opcao5 vec maux = do
  print "OPCAO5"
  prod <- getLine
  
  let result = V.filter ( \compra -> loja compra == prod ) vec
  let soTotal = fmap total result
 
  print ("Quantidade comprada na loja '" ++ prod ++ "': R$ " ++ (formatFloatN (calculaTotal (V.toList soTotal)) 2))                   
  menu vec maux


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

pProds [] = print ""
pProds (x:[]) = do
  print x
pProds (x:xs) = do
  print x
  pProds xs

printProds (x:xs) = do
  let prePrint | length (x:xs) == 0 = "Nao foi comprado nenhum produto ainda"
               | length (x:xs) == 1 = "O produto mais comprado foi: "
               | length (x:xs) > 1 = "Houve empate. Os produtos mais comprados foram: "
  print prePrint
  pProds (x:xs)


opcao6 vec maux = do
  print "OPCAO6"
  
  let produtosDistintos = (nub (V.toList (fmap produto vec)))
      qtdTotalPorProduto = calcProdutos produtosDistintos vec []
  
  printProds $ pegaMaisComprados produtosDistintos qtdTotalPorProduto 0 []

  menu vec maux

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

-- nos prints colocar o nome do produto tb?
-- colocar prints depois dos resultados pra ter um espacinho do menu?