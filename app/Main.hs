module Main where

import Lib


main :: IO()
main = do 
       someFunc
       menu


menu :: IO ()
menu = do
       putStrLn ("Digite uma opcao: ")
       op <- getLine
       let action | op /= "sair" = menu
                  | otherwise = putStrLn ("")
       action



