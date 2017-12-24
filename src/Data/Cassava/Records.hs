{-# LANGUAGE TemplateHaskell #-}

module Data.Cassava.Records where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.ByteString as BL
import qualified Data.ByteString.Char8 as BC
import Data.Csv.Parser as CP
import qualified Data.Vector as V
import Data.List as L
import Data.HashMap.Strict as H
import Data.Csv hiding(Name)
import Data.Attoparsec.ByteString as P


makeField:: String -> Name -> String -> (Name, Bang, Type)
makeField fname ftype suffix = (
  mkName (suffix ++ fname), defaultBang , ConT ftype)
  where
  defaultBang = Bang NoSourceUnpackedness NoSourceStrictness

makeFields:: V.Vector (String, Name) -> String -> V.Vector (Name, Bang, Type)
makeFields fnames_types suffix = V.map makeField' fnames_types
  where
    makeField' = (\(f, t) -> makeField f t suffix)

makeRecord :: String -> [(Name, Bang, Type)] -> DecsQ
makeRecord record_name fields = do
  let record_name' =  mkName record_name
      recc = RecC (record_name') fields
      deriv =  [DerivClause Nothing [ConT ''Show]]
      r = DataD [] (record_name') [] Nothing [recc] deriv
  return [r]

createRecords csvData =
  let p = CP.csvWithHeader CP.defaultDecodeOptions
      e = P.parseOnly p csvData in
    case e of
      Right f -> f
      Left f -> fail  $ "unable to parse" ++ f

inferColumnType :: BL.ByteString -> V.Vector String -> (String, Name)
inferColumnType header column = (BC.unpack header, ''Integer)

inferTypesForHeader :: BL.ByteString -> V.Vector NamedRecord -> V.Vector String
inferTypesForHeader header named_records =
  V.map fn named_records where
  fn r = BC.unpack $ (r ! header)

inferTypes :: Header -> V.Vector NamedRecord -> String -> V.Vector (Name, Bang, Type)
inferTypes headers named_records suffix =
  let columns = V.map (flip inferTypesForHeader named_records) headers
      fieldnames_types = makeFields (V.zipWith inferColumnType headers columns) suffix
  in
    fieldnames_types

makeCsvRecord recordName fileName suffix = do
  csvData <- runIO $ BL.readFile fileName
  let (headers, named_records) = createRecords csvData
  makeRecord recordName (V.toList $ inferTypes headers named_records suffix)
