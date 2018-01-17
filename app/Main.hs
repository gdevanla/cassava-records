{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE DuplicateRecordFields #-}

module Main where

import Data.Cassava.Records
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import Data.Vector as V
import Data.Text as DT
-- import Control.Lens hiding (element)

$(makeCsvRecord "Salaries" "data/salaries.csv" "_" commaOptions)
-- $(makeInstance "Salaries")

-- $(makeCsvRecord "Sec" "SEC_20170802.csv" "_" commaOptions)
-- $(makeCsvRecord "NewR" "SEC_20170802_10.txt" "_tsv" tabOptions)

-- $(makeCsvRecord "NewR" "SEC_20170802_10.csv" "_csv" commaOptions)

myOptions :: Options
myOptions = defaultOptions { fieldLabelModifier = rmUnderscore }
  where
    rmUnderscore ('_':str) = DT.unpack . DT.toUpper . DT.pack $ str
    rmUnderscore str = str

myOptions2 :: Options
myOptions2 = defaultOptions { fieldLabelModifier = rmUnderscore }
  where
    rmUnderscore ('_':str) = DT.unpack . DT.pack $ str
    rmUnderscore str = str

instance ToNamedRecord Salaries where
  toNamedRecord = genericToNamedRecord myOptions

-- $(makeNamedInstances ''NewR)

-- instance ToNamedRecord Salaries where
--   toNamedRecord = genericToNamedRecord myOptions2

instance FromNamedRecord Salaries where
  parseNamedRecord = genericParseNamedRecord myOptions

-- instance DefaultOrdered Salaries where
--   headerOrder = genericHeaderOrder myOptions2


-- instance ToNamedRecord Sec where
--   toNamedRecord = genericToNamedRecord myOptions

-- instance FromNamedRecord Sec where
--   parseNamedRecord = genericParseNamedRecord myOptions

-- instance DefaultOrdered Sec where
--   headerOrder = genericHeaderOrder myOptions

-- makeLenses ''Sec

-- makeLenses ''Salaries

-- loadData fname = do
--   csvData <- BL.readFile fname
--   case decodeByName csvData of
--     Left err -> fail ("Failed to load" Prelude.++ err)
--     Right (_, v::V.Vector NewR) -> return v

loadData fname = do
  csvData <- BL.readFile fname
  case decodeByName csvData of
    Left err -> fail ("Failed to load" Prelude.++ err)
    Right (_, v::V.Vector Salaries) -> return v

-- loadData fname = do
--   csvData <- BL.readFile fname
--   case decodeByName csvData of
--     Left err -> fail ("Failed to load" Prelude.++ err)
--     Right (_, v::V.Vector Sec) -> return v


main :: IO ()
main = do
  -- v <- loadData "SEC_20170802_10.csv"
  v <- loadData "data/salaries.csv"
  putStrLn . show $ v

-- main:: IO ()
-- main = putStrLn ""
