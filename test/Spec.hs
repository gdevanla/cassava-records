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
            ("name", ConT ''DT.Text),
            ("salary", ConT ''Double),
            ("status", ConT ''Bool),
            ("years", ConT ''Integer)]
      let record_name = "TestSimpleR"
      let expected = makeRecord record_name $ makeFields expected_types "_"
      let qdec = makeCsvRecord record_name "test/data/salaries_simple.csv" "_" commaOptions
      dec <- runQ qdec
      expected_dec <- (runQ expected)
      assertEqual "Test Simple" (Prelude.head dec) (Prelude.head expected_dec)
      --   DataD _ name _ _ con derive_clause -> check name con derive
      --   _ -> assertFailure "type not supported"
      -- let check_name exp actual = assertEqual "check name" exp actual
      -- let check_con exp actual = assertEqual "check field types" exp actual
      -- let check name con derive = do (
      --                                  check_name "TestSimpleR" name
      --                                  check_cont expected_con con
      --                                  )


  )

tests :: TestTree
tests = testGroup "Tests" [
  testSimple
  ]

main = defaultMain tests
