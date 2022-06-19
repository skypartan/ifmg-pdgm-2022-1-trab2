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


opcao1 vec = do
  print "OPCAO1"
  prod <- getLine
  
  let result = V.filter ( \compra -> produto compra == prod ) vec 

  let comprado | length result > 0 = "O produto " ++ produto (V.head result) ++ " foi comprado"
               | otherwise = "O produto nao foi comprado"
  
  print comprado

  menu vec

calculaTotal (x:xs) = x + calculaTotal xs 
calculaTotal [] = 0

opcao2 vec = do
  print "OPCAO2"
  prod <- getLine
  
  let result = V.filter ( \compra -> produto compra == prod ) vec
  let soQtd = fmap quantidade result

  let unid | length result > 0 = " " ++ (unidade (V.head result)) ++ "s"
           | otherwise = " unidades"

  print ("Quantidade comprada do produto: " ++ show (calculaTotal (V.toList soQtd)) ++ unid)
  menu vec


formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""

opcao3 vec = do
  print "OPCAO3"
  prod <- getLine
  
  let result = V.filter ( \compra -> produto compra == prod ) vec
  let soQtd = fmap total result


  print ("Valor total comprado do produto: R$ " ++ (formatFloatN (calculaTotal (V.toList soQtd)) 2))
  menu vec


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

opcao4 vec = do
  print "OPCAO4"
  prod <- getLine
  
  let result = V.filter ( \compra -> dia compra == prod ) vec
  let soProdutos = fmap produto result
 
  --MostraProdutos sem duplicatas
  mostraProdutos (nub (V.toList soProdutos)) prod
  --mostraProdutos (V.toList soProdutos) prod
                      
  menu vec


opcao5 vec = do
  print "OPCAO5"
  prod <- getLine
  
  let result = V.filter ( \compra -> loja compra == prod ) vec
  let soTotal = fmap total result
 
  print ("Quantidade comprada na loja '" ++ prod ++ "': R$ " ++ (formatFloatN (calculaTotal (V.toList soTotal)) 2))                   
  menu vec



opcao6 vec = do
  print "OPCAO6"
 
  print (nub (V.toList (fmap produto vec)))

  menu vec

--menu :: IO ()
menu vec = do
  print "Digite uma opcao: "

  op <- getLine
  let action
        | op == "sair" = print ""
        | op == "1" = opcao1 vec
        | op == "2" = opcao2 vec
        | op == "3" = opcao3 vec
        | op == "4" = opcao4 vec
        | op == "5" = opcao5 vec
        | op == "6" = opcao6 vec
        | otherwise = menu vec
  
  action


main :: IO ()
main = do
  bytes <- ByteString.readFile "./input.csv"

  case decodeByName bytes of
    Left string -> fail string
    Right (_header, rows) -> do
      print (rows :: Vector Compra)
      print "\n\n"
      menu rows

-- nos prints colocar o nome do produto tb?
-- colocar prints depois dos resultados pra ter um espacinho do menu?