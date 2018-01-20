{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Map as M
import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BLZ
import Language.Haskell.TH
import Data.Text as DT

import Data.Cassava.Records
import Data.Cassava.Internal.RecordBuilder


-- expectedRecord record_name name_type = RecC (record_name') fields

-- expectedDeriv = [DerivClause Nothing [ConT ''Show, ConT ''Generic, ConT ''Data]]

-- expectedData record_name name_type =
--   DataD [] record_name [] Nothing expectedRecord name_type

testSimple = testCase "testSimpleInput"
  (
    do
      let expected_types = V.fromList  [
            ("emp_no", ConT ''Integer),
            ("name", ConT ''DT.Text),
            ("salary", ConT ''Double),
            ("status", ConT ''Bool),
            ("years", ConT ''Double)]
      let record_name = "TestSimpleR"
      let expected = makeRecord record_name $ makeFields expected_types "_"
      let qdec = makeCsvRecord record_name "test/data/salaries_simple.csv" "_" commaOptions
      dec <- runQ qdec
      expected_dec <- (runQ expected)
      assertEqual "Test Simple" (Prelude.head dec) (Prelude.head expected_dec)
  )


testMixedInput = testCase "testMixedInput"
  (
    do
      let expected_types = V.fromList  [
            ("emp_no", ConT ''Integer),
            ("name",  maybeType ''DT.Text),
            ("salary", maybeType ''Double),
            ("status", maybeType ''Bool),
            ("years", maybeType ''Double)]
      let record_name = "TestMixedR"
      let expected = makeRecord record_name $ makeFields expected_types "_"
      let qdec = makeCsvRecord record_name "test/data/salaries_mixed_input.csv" "_" commaOptions
      dec <- runQ qdec
      expected_dec <- (runQ expected)
      assertEqual "Test Maybe Columns" (Prelude.head dec) (Prelude.head expected_dec)
  )

tests :: TestTree
tests = testGroup "Tests" [
  testSimple,
  testMixedInput
  ]

main = defaultMain tests
