import DSLTokens 
import DSLGrammar
import DSLInterpreter
import System.Environment 
import Control.Exception
import System.IO
import Data.List.Split
import Files

main :: IO()
main = do
    args <- getArgs
    if length args == 0 
        then catch interactive noLex
        else 
            if "-l" `elem` args || "--inline" `elem` args
                then interpretLine (args !! 1)
                else main' (args !! 0) args

interpretLine :: String -> IO()
interpretLine l = do 
    let tokenisedProg = alexScanTokens l
    let parsedProg = parseDSL tokenisedProg
    interpreted <- interpret parsedProg

    putStr(pprint interpreted)

main' :: FilePath -> [String] -> IO()
main' p args = do
    f <- readLines p
    interpretFile f

    where 
        interpretFile :: [String] -> IO()
        interpretFile [] = return ()
        interpretFile [x] = do
            let tokenisedProg = alexScanTokens x
            let parsedProg = parseDSL tokenisedProg
            interpreted <- interpret parsedProg

            if "-p" `elem` args || "--pretty" `elem` args
                then putStr(pprint interpreted)
                else putStrLn(csvprint interpreted)                    

        interpretFile (x:xs) = do
            let tokenisedProg = alexScanTokens x
            let parsedProg = parseDSL tokenisedProg
            interpreted <- interpret parsedProg
            
            return ()

interactive :: IO()
interactive = do 
    putStrLn ("Interactive Mode - enter an expression: ")
    putStr ("> ")
    sourceText <- getLine
    let tokenisedProg = alexScanTokens sourceText
    let parsedProg = parseDSL tokenisedProg
    interpreted <- interpret parsedProg
    putStrLn ("Lexed as " ++ (show tokenisedProg))
    putStrLn ("Parsed as " ++ (show parsedProg))
    putStrLn ("Interpreted as \n" ++ pprint interpreted)
    interactive

noLex :: ErrorCall -> IO()
noLex e = do 
    let err = show e 
    putStrLn("----------------")
    hPutStrLn stderr err
    putStrLn("----------------")
    main