# cassava-records

A library extension for Cassava (Haskell CSV parser library) that
automatically creates a Record given the csv file.

# What is this tool for?

Say you are working on a project that involves processing a number of
comma separated or tab serparated files. Assuming, you are using
cassava for loading the input files, here is a typical workflow you
would follow

a. Inspect the file
b. Create a ```Record``` data type to reflect the columns and types
found in the file
c. Create instances of the Record type that may be required to load
the files with Cassava.

Now, imagine this file you are inspecting to contains tens or hundreds
of columns. Now, as a good Haskeller you will want to automate steps
(a) and (b) to the extend possible. That is precisely, what this
library does.

Cassava-records performs the following tasks. Given, a input file
(command or tab-seperated for example), it reads the whole file,
infers some basic data types for each column and automatically created
a ```Record``` data type using ```Template Haskell```.


# Quick Start

## Example 1 :

Using data/salaries_simple.csv

```
emp_no,name,salary,status,years
1,John Doe,100.0,True,1
2,Jill Doe, 200.10,False,2
3,John Doe Sr,101.0,T,3
4,Jill Doe Sr, 10101.10,f,4.2
5,John Doe Jr,1010101.0,true,5.1
6,Jill Doe Jr, 10101.10,false,6

```

``` haskell

{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE DeriveGeneric #-}

import Data.Cassava.Records
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import Data.Vector as V
import Data.Text as DT

$(makeCsvRecord "Salaries" "data/salaries_simple.csv" "_" commaOptions)

```

The ```makeCsvRecord``` needs take 4 arguments,

1. ```"Salaries"``` :  A ```String``` that will be used as name for
   the ```Record```.
2. ```data/salaries_simple.csv```: path to the input file
3. ```"_"```: string to prefix each field. Useful, if we need to build lens
for this record
4. ```commaOptions```: ```defaultDecodeOptions``` defined
   in ```cassava``` library

If you load this code in ```GHCi```, we will see

``` haskell
1 >:info Salaries
data Salaries
  = Salaries {
    _emp_no:: Integer,
    _name :: Text,
    _salary :: Double,
    _status :: Bool,
    _years:: Double}

```
Note that all column names are in lower case and "_" has been prefixed
to the column names.

To be consistent, ```cassava-record``` converts all column headers to
lower-case before created corresponding field names for each column
header. Therefore, if column headers were all upper-case, we need to
provide a field modifier while creating ```ToNamedRecord```
and ```FromNamedRecord``` instances for ```cassava```.

Note, that if the column headers are mixed case, it become
tricky. Current version of the library does not work very well with
mixed case column headers.

There is a convenience method called ```makeInstances``` that can create
the instances required for ```cassava```.The instances created use the
default ```fieldModifieroptions``` settings shown below.

``` haskell

fieldModifierOptions :: Options
fieldModifierOptions = defaultOptions { fieldLabelModifier = rmUnderscore }
  where
    rmUnderscore ('_':str) = DT.unpack . DT.toUpper . DT.pack $ str
    rmUnderscore str = str

-- the ToNamedRecord and FromNamedRecord are needed by Cassava since
-- we prefix
instance ToNamedRecord Salaries where
  toNamedRecord = genericToNamedRecord fieldModifierOptions

instance FromNamedRecord Salaries where
  parseNamedRecord = genericParseNamedRecord fieldModifierOptions

main :: IO ()
main = do
  v <- loadData "data/salaries_simple.csv":: IO (V.Vector Salaries)
  putStrLn . show $ v
```

In ```GHCi``` we see (formatted for clarity)

``` haskell
2 >loadData "data/salaries.csv"
[Salaries {_emp_no = 1, _name = "John Doe", _salary = 100.0, _status = False, _years = 1.0},
 Salaries {_emp_no = 2, _name = "Jill Doe", _salary = 200.1, _status = False, _years = 2.0},
 Salaries {_emp_no = 3, _name = "John Doe Sr", _salary = 101.0, _status = True, _years = 3.0},
 Salaries {_emp_no = 4, _name = "Jill Doe Sr", _salary = 10101.1, _status = False, _years = 4.2},
 Salaries {_emp_no = 5, _name = "John Doe Jr", _salary = 1010101.0, _status = False, _years = 5.1},
 Salaries {_emp_no = 6, _name = "Jill Doe Jr", _salary = 10101.1, _status = False, _years = 6.0}]
```

Note, the type inference in the above example is as follows:

1. If a column has values from the set {```true```, ```t```, ```false```, ```f```}
   (ignoring case) then the inferred type is ```Bool```.
2. If a column has values that are all numeric, then an ```Integer```
   type is attempted, or else a ```Double``` is infered. For example
   for ```emp_no``` the infered type is a ```Integer``` whereas for ```years``` the type is ```Double```.
3. For all other cases, a ```Text``` type is inferred.

# Example 2 (Missing Values)

The library also supports type inference when values are missing. For example in, data/salaries_mixed_input.csv

```
emp_no,name,salary,status,years
1,John Doe,100.0,True,1
2,Jill Doe, 200.10,False,2
3,John Doe Sr,101.0,T,
4,Jill Doe Sr,10101.10,,4.2
5,John Doe Jr,,true,5.1
6,, 10101.10,false,6
```

the ```status``` for Jill Doe Jr is missing and the ```salary``` for
John Doe Sr is missing. In this case, the type as wrapped in a ```Maybe``` type.

In that case, the record instance we get will be as follows:

``` haskell
3 >:info Salaries
data Salaries
  = Salaries {
    _emp_no:: Integer,
    _name :: Maybe Text,
    _salary :: Maybe Double,
    _status :: Maybe Bool,
    _years:: Maybe Double}
```

Loading this data, would produce the following output

``` haskell
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE DeriveGeneric #-}

import Data.Cassava.Records
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import Data.Vector as V
import Data.Text as DT

$(makeCsvRecord "SalariesMixed" "data/salaries_mixed_input.csv" "_" commaOptions)
$(makeInstance "SalariesMixed")
-- ^ note that we can use this function instead of manually defining
-- all instances required by Cassava

main :: IO ()
main = do
  v <- loadData "data/salaries_mixed_input.csv":: IO (V.Vector SalariesMixed)
  putStrLn . show $ v

  ```

The output will be as follows:

```
[SalariesMixed {_emp_no = 1, _name = Just "John Doe", _salary = Just 100.0, _status = Just False, _years = Just 1.0},
 SalariesMixed {_emp_no = 2, _name = Just "Jill Doe", _salary = Just 200.1, _status = Just False, _years = Just 2.0},
 SalariesMixed {_emp_no = 3, _name = Just "John Doe Sr", _salary = Just 101.0, _status = Just True, _years = Nothing},
 SalariesMixed {_emp_no = 4, _name = Just "Jill Doe Sr", _salary = Just 10101.1, _status = Nothing, _years = Just 4.2},
 SalariesMixed {_emp_no = 5, _name = Just "John Doe Jr", _salary = Nothing, _status = Just False, _years = Just 5.1},
 SalariesMixed {_emp_no = 6, _name = Nothing, _salary = Just 10101.1, _status = Just False, _years = Just 6.0}]

```

Here is a full working code that uses both the examples:

``` haskell
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

-- the following instance is not required, if $(makeInstance Salaries) statement
-- is spliced in (currently commented in the example)
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

```


# Caveats (Or list of future enhancements)

1. The columns names along with prefix should be valid Haskell field
   names. For example, column names cannot have spaces or other
   characters not supported by ```field``` names are not supported.
2. The library loads the whole file during compilation to infer
   types. Given the size of the file, this will increase the compile
   time. Alternative workflows, like stripping the file or dumping the
   created slice into a file is recommended. In the future, the
   makeCsvRecord function can take a parameter to specify the minimum
   number of rows that can be used to infer the types.
3. Supported types are limited. Text, Bool, Integer, Double and the MayBe
   variants of those.
4. Mixed case column headers not automatically supported. A more
   complex form of ```fieldOptionModifiers``` needs to be provided.
5. Currently no options to provide custom types.
