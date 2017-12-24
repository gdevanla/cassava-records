{-#LANGUAGE TemplateHaskell#-}

module Main where

import Data.Cassava.Records

$(makeCsvRecord "NewR" "salaries.csv" "_csv")

main :: IO ()
main = putStrLn ""
