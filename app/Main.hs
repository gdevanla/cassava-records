{-#LANGUAGE TemplateHaskell#-}

module Main where

import Data.Cassava.Records

$(makeCsvRecord "NewR" "salaries.csv" "_csv" commaOptions)

-- $(makeCsvRecord "NewR" "SEC_20170802.csv" "_csv" commaOptions)

-- $(makeCsvRecord "NewR" "SEC_20170802_10.txt" "_tsv" tabOptions)

-- $(makeCsvRecord "NewR" "SEC_20170802_101.csv" "_csv" commaOptions)

main :: IO ()
main = putStrLn ""
