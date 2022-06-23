{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

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
import System.Environment (getArgs)

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


opcao1 vec outFile maux = do
  putStrLn "Digite o nome do produto: "
  prod <- getLine
  --filtra pra pegar so o produto desejado se ele existir
  let result = V.filter ( \compra -> produto compra == prod ) vec
      -- res eh a resposta, ela eh construida de acordo com a presencao ou nao do item procurado
      -- o resultado do filtro anterior
      res | length result > 0 = "O produto '" ++ produto (V.head result) ++ "' foi comprado\n"
          | otherwise = "O produto '" ++ prod ++ "' nao foi comprado\n"
  --printa a resposta
  putStrLn res
  -- continua o loop do programa (volta para o menu incial)
  -- passa como argumento o vetor de compras (vec), o nome para o arquivo de saida (outFile)
  -- e concatena a resposta dessa operacao ao vetor (maux) para que possa ser utilizado
  -- na hora de gerar o relatorio 
  menu vec outFile ((res ++ "\n"):maux)

-- faz o que a funcao 'sum' faz (somar elementos de um vetor)
-- eu criei ela pq nao sabia que ja existia a 'sum'
calculaTotal (x:xs) = x + calculaTotal xs
calculaTotal [] = 0

opcao2 vec outFile maux = do
  putStrLn "Digite o nome do produto: "
  prod <- getLine
  -- filtra pegando so as compras daquele produto
  let result = V.filter ( \compra -> produto compra == prod ) vec
      -- usa fmap para fazer um vetor com as quantidades das compras do array definido anteriormente
      -- (tal array so vai ter compras do produto desejado)
      soQtd = fmap quantidade result
      -- determina a unidade do produto comprado
      unid | length result > 0 = " " ++ unidade (V.head result) ++ "s"
           | otherwise = " unidades"
      -- resposta eh construida
      res = "Quantidade comprada do produto '" ++ prod ++ "': " ++ show (calculaTotal (V.toList soQtd)) ++ unid ++ "\n"

  putStrLn res

  menu vec outFile ((res ++ "\n"):maux)

-- formata o float de entrada para exibir a quantidade de casas decimais desejadas e definidas
-- no argumento 'numOfDecimals'
formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""

opcao3 vec outFile maux = do
  putStrLn "Digite o nome do produto: "
  prod <- getLine
  -- filtra pegando so as compras daquele produto 
  let result = V.filter ( \compra -> produto compra == prod ) vec
      -- das compras daquele produto cria um vetor contendo o total de cada commpra
      soQtd = fmap total result
      -- soma os totais, formata as casas decimais que serao exibidas (2 casas) e constroi a
      -- string de resposta
      res = "Valor total comprado do produto '" ++ prod ++ "': R$ " ++ (formatFloatN (calculaTotal (V.toList soQtd)) 2) ++ "\n"

  putStrLn res

  menu vec outFile ((res ++ "\n"):maux)

--adiciona na string aux os produtos na lista do primeiro parametro e retorna o aux
--utilizado para mostrar os produtos comprados  na data na opcao4
mProdutos (x:[]) aux =
                        (aux ++ x ++ "\n")
mProdutos (x:xs) aux =
                        mProdutos xs (aux ++ x ++ "\n")
-- monta string inicial dependendo se ha ou nao produtos e concatena essa
-- string com a que vem de 'mProdutos'
mostraProdutos (x:xs) d =
                        mProdutos (x:xs) ("Produtos comprados em " ++ d ++ ":\n")
mostraProdutos [] d =
                  ("Nada foi comprado em " ++ d ++ "\n")

opcao4 vec outFile maux = do
  putStrLn "Digite a data desejada no formato dd-mm-AAAA"
  prod <- getLine
  -- filtra pra pegar as compras so naquele dia
  let result = V.filter ( \compra -> dia compra == prod ) vec
      -- pega so o campo produto do vetor anterior
      soProdutos = fmap produto result
      --MostraProdutos sem duplicatas (nub tira as duplicatas)
      res = mostraProdutos (nub (V.toList soProdutos)) prod

  putStrLn res

  menu vec outFile ((res ++ "\n"):maux)


opcao5 vec outFile maux = do
  putStrLn "Digite o nome da loja: "
  prod <- getLine
  -- filtra pegando so as compras da loja
  let result = V.filter ( \compra -> loja compra == prod ) vec
      -- monta um vetor so com os totais de cada compra na loja
      soTotal = fmap total result
      -- monta string de resposta calculando a soma dos totais e formatando o float para
      -- ser exibido com 2 casas decimais
      res = "Quantidade comprada na loja '" ++ prod ++ "': R$ " ++ formatFloatN (calculaTotal (V.toList soTotal)) 2 ++ "\n"

  putStrLn res

  menu vec outFile ((res ++ "\n"):maux)


inverteL (x:xs) aux = inverteL xs (x:aux)
inverteL [] aux = aux

inverteLista (x:xs) = inverteL (x:xs) []
inverteLista [] = []

-- para cada produto passado na lista do primeiro argumento, calcula o total comprado
calcProdutos (x:xs) vec aux =
  let soX = V.filter (\compra -> produto compra == x) vec
      totaisdoX = fmap quantidade soX
  in calcProdutos xs vec ((calculaTotal (V.toList totaisdoX)):aux)

calcProdutos [] _ aux = inverteLista aux

-- determina qual (ou quais no caso de empate) produto foi o mais comprado
pegaMaisComprados (x:xs) (y:ys) maior items
  | y > maior = pegaMaisComprados xs ys y (x:[])
  | y == maior = pegaMaisComprados xs ys y (x:items)
  | otherwise = pegaMaisComprados xs ys maior items

pegaMaisComprados [] [] _ items = items

pProds [] aux = aux

pProds (x:[]) aux =
  aux ++ x ++ "\n"

pProds (x:xs) aux =
  pProds xs (aux ++ x ++ "\n")
--chama 'pProds'. Juntos, constroem a string que exibe qual foi o produto mais comprado
printProds (x:xs) =
  let prePrint | length (x:xs) == 0 = "Nao foi comprado nenhum produto ainda"
               | length (x:xs) == 1 = "O produto mais comprado foi: \n"
               | length (x:xs) > 1 = "Houve empate. Os produtos mais comprados foram: \n"
  in pProds (x:xs) prePrint


opcao6 vec outFile maux = do

  let produtosDistintos = nub (V.toList (fmap produto vec))
      qtdTotalPorProduto = calcProdutos produtosDistintos vec []
      res = printProds $ pegaMaisComprados produtosDistintos qtdTotalPorProduto 0 []

  putStrLn res

  menu vec outFile ((res ++ "\n "):maux)

inverteB :: [[a]] -> [[a]] -> [[a]]
inverteB (x:xs) aux = inverteB xs (x:aux)
inverteB [] aux = aux

--inverte um vetor de vetores, util para colocar as respostas
-- das consultas armazenadas no buffer na ordem certa
inverteBuffer :: [[a]] -> [[a]]
inverteBuffer (x:xs) = inverteB (x:xs) [[]]
inverteBuffer [] = []

printRelat (x:xs) aux = printRelat xs (aux ++ x)
printRelat [] aux = putStrLn aux

opcao7 vec outFile maux = do
  putStrLn "Salvando respostas em um relatorio..."
  putStrLn "relatorio:"
  let relatorio = inverteBuffer maux

  -- aqui que vai escrever num arquivo
  printRelat relatorio ""

  let tryprint | length (concat maux) > 0 = do
                                            writeFile outFile (concat maux)
                                            putStrLn ("Salvo com sucesso no arquivo "++ outFile ++ "\n")
               | otherwise = putStrLn "Nao ha respostas a serem salvas em um arquivo. Tente apos fazer alguma consulta\n"
  tryprint
  menu vec outFile []

--exibe o total comprado em determinada data
opcao8 :: Vector Compra -> String -> [[Char]] -> IO ()
opcao8 vec outFile maux = do
  putStrLn "Informe a data no formato dd-mm-AAAA:"
  d <- getLine

  let result =  V.filter ( \compra -> dia compra == d ) vec
      filt = V.map total result
      tot = sum filt
      res = "Foram comprados R$ " ++ formatFloatN tot 2 ++ " no dia " ++ d ++ "\n"

  putStrLn res


  menu vec outFile ((res ++ "\n "):maux)
  
--exibe o custo total das compras
opcao9 :: Vector Compra -> String -> [[Char]] -> IO ()
opcao9 vec outFile maux = do

  let soTotal = fmap total vec
      res = "O custo total eh de: R$ " ++ formatFloatN (sum soTotal) 2 ++ "\n"

  putStrLn res

  menu vec outFile ((res ++ "\n "):maux)

--menu :: IO ()
menu vec outFile maux = do
  putStrLn $ "**********MENU**********\n" ++
            "Digite o numero de uma das seguintes opcoes ou digite 'sair' para encerrar o programa: \n" ++
            "1 - Consultar se um produto foi comprado.\n" ++
            "2 - Consultar a quantidade total comprada de um produto.\n" ++
            "3 - Consultar o valor total comprado de um produto.\n" ++
            "4 - Consultar quais produtos foram comprados em uma determinada data.\n" ++
            "5 - Consultar qual o total de compras em uma determinada loja.\n" ++
            "6 - Consultar qual foi o produto mais comprado.\n" ++
            "7 - Salvar as respostas das consultas em um arquivo texto.\n" ++
            "8 - Consultar o custo total com produtos em determinado dia.\n" ++
            "9 - Consultar o custo total das compras."

  op <- getLine
  let action
        | op == "sair" = putStrLn "Finalizado com sucesso"
        | op == "1" = opcao1 vec outFile maux
        | op == "2" = opcao2 vec outFile maux
        | op == "3" = opcao3 vec outFile maux
        | op == "4" = opcao4 vec outFile maux
        | op == "5" = opcao5 vec outFile maux
        | op == "6" = opcao6 vec outFile maux
        | op == "7" = opcao7 vec outFile maux
        | op == "8" = opcao8 vec outFile maux
        | op == "9" = opcao9 vec outFile maux
        | otherwise = menu vec outFile maux

  action


main :: IO ()
main = do
  -- pega os nomes para os arquivos de entrada e saida
  -- caso nao sejam passados, usa nomes padrao.
  args <- getArgs
  let inFile = if null args then "input.csv" else head args
      outFile = if length args == 2 then args !! 1 else "output.txt"

  putStrLn ("arquivo de entrada " ++ inFile)
  putStrLn ("arquivo de saída " ++ outFile)

  bytes <- ByteString.readFile inFile

  case decodeByName bytes of
    Left string -> fail string
    Right (_header, rows) -> do
      print (rows :: Vector Compra)
      putStrLn "\nBanco de dados carregado\n"
      menu rows outFile []
