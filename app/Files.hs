module Files where 

import System.Environment 
import Control.Exception
import System.IO
import Data.List.Split
import System.Exit

type File = [Line]
type Line = [String]

-- | Reads lines from a file under @path@
--
-- >>> readLines "A.csv"
-- ["Jian,Shi"]
readLines :: FilePath -> IO[String]
readLines = fmap lines . readFile

-- | Reads a CSV file under @path@ and returns its contents
--
-- >>> readCSVFile "A.csv"
-- [["Jian", "Shi"]]
readCSVFile :: String -> IO(File)
readCSVFile path = readXSVFile path ','

-- | Reads any file with a regular @delimiter@ under @path@ and returns its contents
--
-- >>> readXSVFile "A.csv" ','
-- [["Jian", "Shi"]]
readXSVFile :: String -> Char -> IO(File)
readXSVFile path delimiter = do
    fileContents <- readLines path
    validate <- validateXSVFile delimiter fileContents
    if fileContents == [""] 
        then do return []
        else do return $ map (splitOn [delimiter]) fileContents

-- | Validates a CSV file regarding arity
-- 
-- >>> validateCSVFile [["Jian,Shi"], ["Julian,Rathke"]]
-- True
validateCSVFile :: [String] -> IO(Bool)
validateCSVFile lines = validateXSVFile ',' lines

-- | Validates any file with a regular @delimiter@ file regarding arity
-- 
-- >>> validateXSVFile ',' [["Jian,Shi"], ["Julian,Rathke"]]
-- True
validateXSVFile :: Char -> [String] -> IO(Bool)
validateXSVFile delimiter (l:ls) = do
    let delimiterCounts = map (countChars delimiter) ls
    if and $ map (== countChars delimiter l) delimiterCounts
    then return True
    else die "Not a valid XSV"

-- | Count char occurences in a line
-- 
-- >>> countChars 'a' "abcabca"
-- 3
countChars :: Char -> String -> Int
countChars c str = length $ filter (== c) str
