{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.String
import Text.Read
import Data.Char
import GHC.Generics (Generic)
import Data.Text as DT

makeField:: String -> Type -> String -> (Name, Bang, Type)
makeField fname ftype suffix = (
  mkName ("_" ++ fname), defaultBang , ftype)
  where
  defaultBang = Bang NoSourceUnpackedness NoSourceStrictness

makeFields:: V.Vector (String, Type) -> String -> V.Vector (Name, Bang, Type)
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

createRecords csvData options =
  let p = CP.csvWithHeader options
      e = P.parseOnly p csvData in
    case e of
      Right f -> f
      Left f -> fail  $ "unable to parse" ++ f

inferColumnType :: BL.ByteString -> V.Vector String -> (String, Type)
inferColumnType header column = (BC.unpack header, inferMajorityType column)

isInteger s = case reads s :: [(Integer, String)] of
    [(_, "")] -> True
    _         -> False

isDouble s = case reads s :: [(Double, String)] of
    [(_, "")] -> True
    _         -> False

isNumeric :: String -> Bool
isNumeric s = isInteger s || isDouble s

-- isBool :: String -> Bool
-- isBool c = let x = toLower c in
--   x == "t" || x == "f" ||  x == "true" || x == "false"


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

inferMajorityType column =
  majority_types types'
  where
    types = V.map find_types column
    types' = V.filter (\t -> t /= ''Empty) types
    non_types' = V.filter (\t -> t == ''Empty) types
    find_types c
      | isNumeric c = ''Double
--      | isBool c  = ''Bool
      | c == "" = ''Empty
      | otherwise = ''String
    majority_types t1
      | (V.all (\t -> t == ''Double) t1) && V.length non_types' > 0 = maybeType ''Double
      | V.all (\t -> t == ''Double) t1 = ConT ''Double
      | (V.all (\t -> t == ''Bool) t1) && V.length non_types' > 0 = maybeType ''Bool
      | V.all (\t -> t == ''Bool) t1 = ConT ''Bool
      | V.length non_types' > 0 = maybeType ''String
      | otherwise = ConT ''String


collectColumns :: BL.ByteString -> V.Vector NamedRecord -> V.Vector String
collectColumns header named_records =
  V.map fn named_records where
  fn r = BC.unpack $ (r ! header)

inferTypes :: Header -> V.Vector NamedRecord -> String -> V.Vector (Name, Bang, Type)
inferTypes headers named_records suffix =
  let columns = V.map (flip collectColumns named_records) headers
      fieldnames_types = makeFields (V.zipWith inferColumnType headers columns) suffix
  in
    fieldnames_types

tabOptions = defaultDecodeOptions {
  decDelimiter = fromIntegral (ord '\t')
  }

commaOptions = defaultDecodeOptions

makeCsvRecord recordName fileName suffix options = do
  csvData <- runIO $ BL.readFile fileName
  let (headers, named_records) = createRecords csvData options
  makeRecord recordName (V.toList $ inferTypes headers named_records suffix)
