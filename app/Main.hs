{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module Main where

import Data.Csv (FromNamedRecord)
import Data.Text (Text)
import Data.Map (Map)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Csv             as Csv
import qualified Data.Vector          as Vector

data Compra = Compra
   { dia :: Text
    ,produto :: Text
    ,loja :: Text
    ,quantidade :: Integer
    ,unidade :: Text
    ,preco :: Double
    ,total :: Double
   } deriving (Generic, FromNamedRecord, Show)

teste (x:xs) = do 
               print ("***" ++ show x ++ "***")
               teste xs
teste [] = print ""


--menu :: IO ()
menu vec = do
       print ("Digite uma opcao: ")
       op <- getLine
       let action | op == "sair" = print ""
                  | op == "1" = do 
                                  print "OPCAO1" 
                                  prod <- getLine
                                  --filter ( \compra -> produto compra == prod ) vec
                                  menu vec
                  | otherwise = menu vec 
       action


main :: IO()
main = do 
       bytes <- ByteString.readFile "./input.csv"

       case Csv.decodeByName bytes of
           Left string           -> fail string
           Right (_header, rows) -> do
--                 print (rows)
--                 print (rows :: Vector (Map Text Text))
--                 print ((fmap loja rows))
--                 teste (Vector.toList rows)
                 print (rows :: Vector Compra)
                 print "//"

                 menu rows
                                  






