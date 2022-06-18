{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE OverloadedStrings #-}

module Lib
  ( main,
  )
where

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
  let p = V.head result

  print ("O produto " ++ produto p ++ " foi comprado")

  menu vec


-- opcao2 vec = do
--   print "OPCAO2"
--   prod <- getLine
  
--   let result = V.filter ( \compra -> produto compra == prod ) vec

--   print ("O produto " ++ produto p ++ " foi comprado")
--   menu vec


--menu :: IO ()
menu vec = do
  print "Digite uma opcao: "

  op <- getLine
  let action
        | op == "sair" = print ""
        | op == "1" = opcao1 vec
        -- | op == "2" = opcao2 vec
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
