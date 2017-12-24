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

data Format = D | S | L String

-- printf :: String -> ExpQ
-- printf s = gen (parse s)

-- parse :: String  -> [Format]
-- parse s = case s of
--             "D" -> [D]
--             "S" -> [S]
--             _ -> [L s]

-- gen :: [Format]  -> ExpQ
-- gen [D] = [| \n -> show n |]
-- gen [S] = [| \s -> s |]
-- gen [L s] = [| s |]

-- -- sel (4 More flexible construction)

-- sel :: Int -> Int -> ExpQ
-- sel i n = lamE [varP (mkName "x")] (caseE (varE (mkName "x")) [alt]) where
--   alt:: MatchQ
--   alt = match pat rhs []

--   pat:: PatQ
--   pat = tupP (map varP as)

--   rhs = normalB $ varE (as !! (i - 1))

--   as:: [Name]
--   as = [mkName ("a"++ show i) |  i <- [1..n]]

makeField:: String -> Name -> String -> (Name, Bang, Type)
makeField fname ftype suffix = (
  mkName (suffix ++ fname), Bang NoSourceUnpackedness NoSourceStrictness, ConT ftype)

makeFields:: V.Vector (String, Name) -> String -> V.Vector (Name, Bang, Type)
makeFields fnames_types suffix = V.map (\(f, t) -> makeField f t suffix) fnames_types

makeRecord :: String -> [(Name, Bang, Type)] -> DecsQ
makeRecord record_name fields = do
  let r = DataD [] (
        mkName  record_name) [] Nothing [
        RecC (mkName record_name) fields] [DerivClause Nothing [ConT ''Show]]
  return [r]

-- makeRecord :: [(String, String)] -> DecsQ
-- makeRecord x = do
--   let r = DataD [] (mkName "X1") [] Nothing [RecC (mkName "X") [(mkName "x", Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Integer)]] [ConT ''Show]
--   return [r]

createRecords csvData =
  let p = CP.csvWithHeader CP.defaultDecodeOptions
      e = P.parseOnly p csvData in
    case e of
      Right f -> f
      Left f -> fail "unable to parse"

inferColumnType :: BL.ByteString -> V.Vector String -> (String, Name)
inferColumnType header column = (BC.unpack header, ''Integer)

inferTypesForHeader :: BL.ByteString -> V.Vector NamedRecord -> V.Vector String
inferTypesForHeader header named_records =
  V.map fn named_records where
  fn r = BC.unpack $ (r ! header)
  -- fn s = check $ H.lookup header s where
  --   check a = case a of
  --     Just s1 -> BC.unpack s1
  --     Nothing -> fail "duh"

inferTypes :: Header -> V.Vector NamedRecord -> String -> V.Vector (Name, Bang, Type)
inferTypes headers named_records suffix =
  let columns = V.map (flip inferTypesForHeader named_records) headers
      fieldnames_types = makeFields (V.zipWith inferColumnType headers columns) suffix
  in
    fieldnames_types


makeCsvRecord recordName fileName suffix = do
  csvData <- runIO $ BL.readFile fileName
  let (headers, named_records) = createRecords csvData
  --let fieldnames_types = V.toList $ V.map (\h -> ("_" ++ BC.unpack h, ''Integer)) headers
  --makeRecord "NewR" (makeFields fieldnames_types)
  makeRecord recordName (V.toList $ inferTypes headers named_records suffix)
