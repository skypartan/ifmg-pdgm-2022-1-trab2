{-# LANGUAGE OverloadedStrings #-}

module Lib (
    someFunc
) where


import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V



data Compra = Compra
  { 
    dia :: !String,
    produto :: !String,
    loja :: !String,
    quantidade :: !Int,
    unidade :: !String,
    preco :: !Float,
    total :: !Float
  } deriving (Show)

instance FromNamedRecord Compra where
  parseNamedRecord r = Compra
    <$> r .: "dia"
    <*> r .: "produto"
    <*> r .: "loja"
    <*> r .: "quantidade"
    <*> r .: "unidade"
    <*> r .: "preco"
    <*> r .: "total"


someFunc :: IO ()
someFunc = do
  csvData <- BL.readFile "input.csv"
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, v) -> V.forM_ v $ \p ->
      putStrLn $ produto p ++ " comprado"
