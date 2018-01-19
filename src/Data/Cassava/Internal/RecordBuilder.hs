{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : RecordBuilder.hs
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

module Data.Cassava.Internal.RecordBuilder where

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

{-| Create a field name and type tuple that will be used with RecC to
create a Record.
-}
makeField:: BC.ByteString -> Type -> String -> (Name, Bang, Type)
makeField fname ftype prefix = (
  mkName (fname'), defaultBang , ftype)
  where
  defaultBang = Bang NoSourceUnpackedness NoSourceStrictness
  fname' = prefix ++ (DT.unpack $ DT.toLower $ DTE.decodeUtf8 $ fname)

-- Create the list of fields that will form a Record
makeFields:: V.Vector (BC.ByteString, Type) -> String -> V.Vector (Name, Bang, Type)
makeFields fnames_types prefix = V.map makeField' fnames_types
  where
    makeField' = (\(f, t) -> makeField f t prefix)

-- Return the expression that contains the Record declaration
makeRecord :: String -> V.Vector (Name, Bang, Type) -> DecsQ
makeRecord record_name fields = do
  let record_name' =  mkName record_name
      recc = RecC (record_name') $ V.toList fields
      deriv =  [DerivClause Nothing [ConT ''Show, ConT ''Generic, ConT ''Data]]
      r = DataD [] (record_name') [] Nothing [recc] deriv
  return [r]

-- Parses the file and returns the Header and Data from he input file
createRecords ::
  BC.ByteString -> DecodeOptions -> (Header, V.Vector NamedRecord)
createRecords csvData options =
  let p = CP.csvWithHeader options
      e = P.parseOnly p csvData in
    case e of
      Right f -> f
      Left f -> fail $ "unable to parse" ++ f

-- Infer the type for the given column
inferColumnType :: BL.ByteString -> V.Vector BC.ByteString -> (BC.ByteString, Type)
inferColumnType header column = (header, inferMajorityType column)

-- Check to see if that numeric value can be an Integer
isInteger s = case reads s :: [(Integer, String)] of
    [(_, "")] -> True
    _         -> False

-- Check to see if the value can be a Double
isDouble s = case reads s :: [(Double, String)] of
    [(_, "")] -> True
    _         -> False

-- Check to see if value is Numeric
isNumeric :: String -> Bool
isNumeric s = isInteger s || isDouble s

-- Check to see if value can be a Bool
isBool :: String -> Bool
isBool c = let x = fmap DC.toLower c
  in
  x == "t" || x == "f" ||  x == "true" || x == "false"

{-|
Instances to support conversion to Bool type. Cassava currently does not
provide an instance for Bool.
-}
instance ToField Bool where
  toField True = "True"
  toField False = "False"

{-|
Instances to support creation of Bool type fields. Cassava currently does not
provide an instance for Bool.
-}
instance FromField Bool where
  parseField field = do
    let s' = DT.toLower . DTE.decodeUtf8 $ field
    if s' == "t" || s' == "True" then return True
      else return False

data Empty = Empty

maybeType ftype = AppT (ConT ''Maybe) (ConT ftype)

inferMajorityType :: V.Vector BC.ByteString -> Type
inferMajorityType column =
  majority_types types'
  where
    types = V.map find_types column
    types' = V.filter (\t -> t /= ''Empty) types
    non_types' = V.filter (\t -> t == ''Empty) types
    find_types c
      | isInteger (DT.unpack . DTE.decodeUtf8 $ c) = ''Integer
      | isDouble (DT.unpack . DTE.decodeUtf8 $ c) = ''Double
      | isBool (DT.unpack . DTE.decodeUtf8 $ c)  = ''Bool
      | c == "" = ''Empty
      | otherwise = ''Text
    doubleOrInteger t = t == ''Double || t == ''Integer
    majority_types t1
      | (V.all (\t -> t == ''Integer) t1) && V.length non_types' > 0 = maybeType ''Integer
      | V.all (\t -> t == ''Integer) t1 = ConT ''Integer
      | (V.all doubleOrInteger t1) && V.length non_types' > 0 = maybeType ''Double
      | V.all doubleOrInteger t1 = ConT ''Double
      | (V.all (\t -> t == ''Bool) t1) && V.length non_types' > 0 = maybeType ''Bool
      | V.all (\t -> t == ''Bool) t1 = ConT ''Bool
      | V.length non_types' > 0 = maybeType ''Text
      | otherwise = ConT ''Text


collectColumns :: BL.ByteString -> V.Vector NamedRecord -> V.Vector BC.ByteString
collectColumns header named_records =
  V.map fn named_records where
  fn r = r ! header

inferTypes :: Header -> V.Vector NamedRecord -> String -> V.Vector (Name, Bang, Type)
inferTypes headers named_records suffix =
  let columns = V.map (flip collectColumns named_records) headers
      fieldnames_types = makeFields (V.zipWith inferColumnType headers columns) suffix
  in
    fieldnames_types


-- defaultFieldNameOptions :: Options
-- defaultFieldNameOptions = defaultOptions { fieldLabelModifier = rmUnderscore }
--   where
--     rmUnderscore ('_':str) = DT.unpack . DT.pack $ str
--     rmUnderscore str = str


-- makeInstance :: String -> DecsQ
-- makeInstance recordName = [d|
--    instance ToNamedRecord $(conT (mkName recordName)) where
--         toNamedRecord = genericToNamedRecord $ defaultFieldNameOptions
--    instance FromNamedRecord $(conT (mkName recordName)) where
--         parseNamedRecord = genericParseNamedRecord $ defaultFieldNameOptions
--    instance DefaultOrdered $(conT (mkName recordName)) where
--         headerOrder = genericHeaderOrder $ defaultFieldNameOptions
--    |]
