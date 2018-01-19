{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Records.hs
Description : Using Template Haskell this module auto create Record
              types by inferring types from the provided csv or tab separated file.
Copyright   : (c) Guru Devanla 2018
License     : MIT
Maintainer  : grdvnl@gmail.com
Stability   : experimental


This module provides an easy way to explore input files that may have numerous columns
by helping create a Record types by guessing the types. That information can be used
as is or persisted to a file so that other customizations can be performed.
-}

module Data.Cassava.Records
  (
    -- ** Creating Record types
    -- $makeCsvRecord
    makeCsvRecord
    -- $commaOptions
  , commaOptions
    -- $tabOptions
  , tabOptions
  -- $makeInstance
  , makeInstance
  -- $loadData
  , loadData
  )
where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.ByteString as BL
import qualified Data.ByteString.Lazy as BLZ
import qualified Data.ByteString.Char8 as BC
import Data.Csv.Parser as CP
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import Data.List as L
import Data.HashMap.Strict as H
import Data.Csv hiding(Name)
import Data.Attoparsec.ByteString as P
import Data.Attoparsec.Text as AT
import Data.String
import Text.Read
import qualified Data.Char as DC
import GHC.Generics (Generic)
import Data.Text as DT
import qualified Data.Text.Encoding as DTE
import Data.Data

import Data.Cassava.Internal.RecordBuilder


defaultFieldNameOptions :: Options
defaultFieldNameOptions = defaultOptions { fieldLabelModifier = rmUnderscore }
  where
    rmUnderscore ('_':str) = DT.unpack . DT.pack $ str
    rmUnderscore str = str

{-| Convinience method that creates the default instances required by
Cassava. The generated methods assumed fields are prefixed with "_".

For example, if the column header in the input file have upper case or mixed case
the names will not directly match with field names in the record. In that case
explicity instances have to be provided manually and the field modifiers provided accordingly.

For example, if the columns in the input file have all headers listed in upper case,
since the field names are all lower case, the defaultFieldNameOptions function would look like
this

@

defaultFieldNameOptions :: Options
defaultFieldNameOptions = defaultOptions { fieldLabelModifier = rmUnderscore }
  where
    rmUnderscore ('_':str) = DT.unpack . DT.toUpper . DT.pack $ str
    rmUnderscore str = str
@

Note the DT.toUpper call to convert the field names to upper case before comparing to
'NamedRecords'

-}
makeInstance :: String -- ^ name of record for which the instance needs to be created
             -> DecsQ
makeInstance recordName = [d|
   instance ToNamedRecord $(conT (mkName recordName)) where
        toNamedRecord = genericToNamedRecord $ defaultFieldNameOptions
   instance FromNamedRecord $(conT (mkName recordName)) where
        parseNamedRecord = genericParseNamedRecord $ defaultFieldNameOptions
   instance DefaultOrdered $(conT (mkName recordName)) where
        headerOrder = genericHeaderOrder $ defaultFieldNameOptions
   |]


-- $tabOptions
{-| Provides a default 'DecodeOptions' for tab separated input files
-}
tabOptions :: DecodeOptions
tabOptions = defaultDecodeOptions {
  decDelimiter = fromIntegral (DC.ord '\t')
  }

-- $commaOptions
{-| Provides a default 'DecodeOptions' for comma separated input files
-}
commaOptions :: DecodeOptions
commaOptions = defaultDecodeOptions

-- $makeCsvRecord
{-|
Makes the Record that reflects the types inferred from the input file.
-}
makeCsvRecord :: String -- ^ Name to use for the Record type being created
              -> FilePath  -- ^ File path of input file
              -> String -- ^ Prefix to be used to field names. Recommended to use "_" to work well with Lens
              -> DecodeOptions -- ^ 'DecodeOptions' as required by Cassava to read the input file
              -> DecsQ
makeCsvRecord recordName fileName prefix decodeOptions = do
  csvData <- runIO $ BL.readFile fileName
  let (headers, named_records) = createRecords csvData decodeOptions
  makeRecord recordName (inferTypes headers named_records prefix)

-- $loadData
{-|
Helper function to load the data from the the provided file path
-}
loadData :: (FromNamedRecord a)
         => FilePath  -- ^ Path of the file to be loaded
         -> IO (V.Vector a) -- ^ a will be of a Record type
loadData file_path = do
  csvData <- BLZ.readFile file_path
  case decodeByName csvData of
    Left err -> fail ("Faled to load" Prelude.++ err)
    Right (_, v) -> return v
