{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- Documentation
-- Cleanup import creation
--

module Data.Cassava.Records where

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


makeField:: BC.ByteString -> Type -> String -> (Name, Bang, Type)
makeField fname ftype prefix = (
  mkName (fname'), defaultBang , ftype)
  where
  defaultBang = Bang NoSourceUnpackedness NoSourceStrictness
  fname' = prefix ++ (DT.unpack $ DT.toLower $ DTE.decodeUtf8 $ fname)

makeFields:: V.Vector (BC.ByteString, Type) -> String -> V.Vector (Name, Bang, Type)
makeFields fnames_types suffix = V.map makeField' fnames_types
  where
    makeField' = (\(f, t) -> makeField f t suffix)

makeRecord :: String -> [(Name, Bang, Type)] -> DecsQ
makeRecord record_name fields = do
  let record_name' =  mkName record_name
      recc = RecC (record_name') fields
      deriv =  [DerivClause Nothing [ConT ''Show, ConT ''Generic]]
      r = DataD [] (record_name') [] Nothing [recc] deriv
  return [r]

createRecords ::
  BC.ByteString -> DecodeOptions -> (Header, V.Vector NamedRecord)
createRecords csvData options =
  let p = CP.csvWithHeader options
      e = P.parseOnly p csvData in
    case e of
      Right f -> f
      Left f -> fail  $ "unable to parse" ++ f

inferColumnType :: BL.ByteString -> V.Vector BC.ByteString -> (BC.ByteString, Type)
inferColumnType header column = (header, inferMajorityType column)

isInteger s = case reads s :: [(Integer, String)] of
    [(_, "")] -> True
    _         -> False

isDouble s = case reads s :: [(Double, String)] of
    [(_, "")] -> True
    _         -> False

isNumeric :: String -> Bool
isNumeric s = isInteger s || isDouble s

isBool :: String -> Bool
isBool c = let x = fmap DC.toLower c
  in
  x == "t" || x == "f" ||  x == "true" || x == "false"

instance ToField Bool where
  toField True = "True"
  toField False = "False"

instance FromField Bool where
  parseField field = do
    let s' = DT.toLower . DTE.decodeUtf8 $ field
    if s' == "t" || s' == "True" then return True
      else return False


-- data Person = Person { name :: !Text, age :: !Int }

-- instance FromNamedRecord Person where
--   parseNamedRecord m = Person <$>
--                        m .: "name" <*>
--                        m .: "age"

-- instance FromNamedRecord X where
--   parseNamedRecord m = _

-- class CRecords a where
--   parseCField :: Field -> Parse a
--   parseCField f = parseField f

data Empty = Empty

--mayBeDouble = AppT (ConT ''Maybe) (ConT ''Double)

maybeType ftype = AppT (ConT ''Maybe) (ConT ftype)

inferMajorityType :: V.Vector BC.ByteString -> Type
inferMajorityType column =
  majority_types types'
  where
    types = V.map find_types column
    types' = V.filter (\t -> t /= ''Empty) types
    non_types' = V.filter (\t -> t == ''Empty) types
    find_types c
      | isNumeric (DT.unpack . DTE.decodeUtf8 $ c) = ''Double
      | isBool (DT.unpack . DTE.decodeUtf8 $ c)  = ''Bool
      | c == "" = ''Empty
      | otherwise = ''Text
    majority_types t1
      | (V.all (\t -> t == ''Double) t1) && V.length non_types' > 0 = maybeType ''Double
      | V.all (\t -> t == ''Double) t1 = ConT ''Double
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

tabOptions :: DecodeOptions
tabOptions = defaultDecodeOptions {
  decDelimiter = fromIntegral (DC.ord '\t')
  }


commaOptions :: DecodeOptions
commaOptions = defaultDecodeOptions

defaultFieldNameOptions :: Options
defaultFieldNameOptions = defaultOptions { fieldLabelModifier = rmUnderscore }
  where
    rmUnderscore ('_':str) = DT.unpack . DT.pack $ str
    rmUnderscore str = str


makeInstance :: String -> DecsQ
makeInstance recordName = [d|
   instance ToNamedRecord $(conT (mkName recordName)) where
        toNamedRecord = genericToNamedRecord $ defaultFieldNameOptions
   instance FromNamedRecord $(conT (mkName recordName)) where
        parseNamedRecord = genericParseNamedRecord $ defaultFieldNameOptions
   instance DefaultOrdered $(conT (mkName recordName)) where
        headerOrder = genericHeaderOrder $ defaultFieldNameOptions
   |]

makeCsvRecord ::
  String -> FilePath -> String -> DecodeOptions -> Q [Dec]
makeCsvRecord recordName fileName prefix decodeOptions = do
  csvData <- runIO $ BL.readFile fileName
  let (headers, named_records) = createRecords csvData decodeOptions
  makeRecord recordName (V.toList $ inferTypes headers named_records prefix)


loadData fname = do
  csvData <- BLZ.readFile fname
  case decodeByName csvData of
    Left err -> fail ("Faled to load" Prelude.++ err)
    Right (_, v) -> return v
