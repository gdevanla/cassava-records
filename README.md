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

Using data/salaries.csv

NAME,SALARY,STATUS
John Doe,100.0,True
Jill Doe, 200.10,False
John Doe Sr,101.0,T
Jill Doe Sr, 10101.10,f
John Doe Jr,1010101.0,true
Jill Doe Jr, 10101.10,false

``` haskell

{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE DeriveGeneric #-}

import Data.Cassava.Records
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import Data.Vector as V
import Data.Text as DT

$(makeCsvRecord "Salaries" "data/salaries.csv" "_" commaOptions)

```

The ```makeCsvRecord``` needs take 4 arguments,

1. ```"Salaries"``` :  A ```String``` that will be used as name for
   the ```Record```.
2. ```data/salaries.csv```: path to the input file
3. ```"_"```: string to prefix each field. Useful, if we need to build lens
for this record
4. ```commaOptions```: ```defaultDecodeOptions``` defined
   in ```cassava``` library

If you load this code in ```GHCi```, we will see

``` haskell
1 >:info Salaries
data Salaries
  = Salaries {_name :: Text, _salary :: Double, _status :: Bool}

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

``` haskell

fieldModifierOptions :: Options
fieldModifierOptions = defaultOptions { fieldLabelModifier = rmUnderscore }
  where
    rmUnderscore ('_':str) = DT.unpack . DT.toUpper . DT.pack $ str
    rmUnderscore str = str

instance ToNamedRecord Salaries where
  toNamedRecord = genericToNamedRecord fieldModifierOptions

instance FromNamedRecord Salaries where
  parseNamedRecord = genericParseNamedRecord fieldModifierOptions

--- This is not required. This instance is used in the example
--- to help in formatting. Tabulate available in pptable package
instance T.Tabulate Salaries

--- Now load the data

loadData fname = do
  csvData <- BL.readFile fname
  case decodeByName csvData of
    Left err -> fail ("Failed to load" Prelude.++ err)
    Right (_, v::V.Vector Salaries) -> return v

main :: IO ()
main = do
  v <- loadData "data/salaries.csv"
  putStrLn . show $ v
```

In ```GHCi``` we see (formatted for clarity)

``` haskell
15 >loadData "data/salaries.csv"
1 >loadData "data/salaries.csv"
[Salaries {_name = "John Doe", _salary = 100.0, _status = False},
 Salaries {_name = "Jill Doe", _salary = 200.1, _status =False},
 Salaries {_name = "John Doe Sr", _salary = 101.0, _status =True},
 Salaries {_name = "Jill Doe Sr", _salary = 10101.1, _status =False},
 Salaries {_name = "John Doe Jr", _salary = 1010101.0, _status=False},
 Salaries {_name = "Jill Doe Jr", _salary = 10101.1, _status= False}]
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
3. Supported types are limited. Text, Bool, Double and the MayBe
   variants of those.
4. Mixed case column headers not automatically supported. A more
   complex form of ```fieldOptionModifiers``` needs to be provided.
5. All numeric value column are inferred as ```Double```. That is
   possiblity of an ```Int``` type is ignored.
6. No way to provide custom types.
