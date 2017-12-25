{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Cassava.Records
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import Data.Vector as V
import Data.Text as DT

-- $(makeCsvRecord "NewR" "salaries.csv" "_csv" commaOptions)

-- $(makeCsvRecord "NewR" "SEC_20170802.csv" "_csv" commaOptions)

-- $(makeCsvRecord "NewR" "SEC_20170802_10.txt" "_tsv" tabOptions)

$(makeCsvRecord "NewR" "SEC_20170802_10.csv" "_csv" commaOptions)

myOptions :: Options
myOptions = defaultOptions { fieldLabelModifier = rmUnderscore }
  where
    rmUnderscore ('_':str) = str

instance ToNamedRecord NewR where
  toNamedRecord = genericToNamedRecord myOptions

instance FromNamedRecord NewR where
  parseNamedRecord = genericParseNamedRecord myOptions

instance DefaultOrdered NewR where
  headerOrder = genericHeaderOrder myOptions

main :: IO ()
main = do
  csvData <- BL.readFile "SEC_20170802_10.csv"
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, v::V.Vector NewR) -> putStrLn $ show v
