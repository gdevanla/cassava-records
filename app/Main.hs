{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Cassava.Records
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import Data.Vector as V
import Data.Text as DT
import qualified Text.PrettyPrint.Tabulate as T
import Language.Haskell.TH
-- import Control.Lens hiding (element)

$(makeCsvRecord "Salaries" "data/salaries_simple.csv" "_" commaOptions)
-- $(makeInstance "Salaries")

$(makeCsvRecord "SalariesMixed" "data/salaries_mixed_input.csv" "_" commaOptions)
$(makeInstance "SalariesMixed")

-- the following instance is not required, if $(makeInstance) call is spliced in
myOptions :: Options
myOptions = defaultOptions { fieldLabelModifier = rmUnderscore }
  where
    rmUnderscore ('_':str) = DT.unpack . DT.toUpper . DT.pack $ str
    rmUnderscore str = str

instance ToNamedRecord Salaries where
  toNamedRecord = genericToNamedRecord myOptions

instance FromNamedRecord Salaries where
  parseNamedRecord = genericParseNamedRecord myOptions

main :: IO ()
main = do
  v <- loadData "data/salaries_simple.csv" :: IO (V.Vector Salaries)
  v1 <- loadData "data/salaries_mixed_input.csv" :: IO (V.Vector SalariesMixed)
  putStrLn . show $ v
  putStrLn . show $ v1
