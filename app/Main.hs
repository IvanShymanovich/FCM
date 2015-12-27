module Main (main) where

import System.Console.GetOpt
import System.Environment( getArgs, getProgName )
import Data.Maybe ( fromMaybe, isNothing, fromJust )
--import ParserCSV ( parseCSV )
import System.IO.Error
import Control.Exception
-- from cassava
import Data.Csv
import Lib
import FCM.ParserCSV 
import FCM.FCM

data Options = Options {
    optInput                :: Maybe FilePath
    , optOutput             :: Maybe FilePath
    , optCount              :: Int
    , optPrecision          :: Double
    , optMethod             :: Char
    , optSeparator          :: String
    , optSkipHeadLine       :: Bool
    , optSkipFirstColumn    :: Bool
    , optSkipLastColumn     :: Bool
    , optIsRandMatrix       :: Bool
} deriving Show

defaultOptions = Options {
    optInput                = Nothing
    , optOutput             = Nothing
    , optCount              = 5
    , optPrecision          = 0.0001
    , optMethod             = 'H'
    , optSeparator          = ","
    , optSkipHeadLine       = False
    , optSkipFirstColumn    = False
    , optSkipLastColumn     = False
    , optIsRandMatrix       = True
}

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['i'] ["input"]
        (ReqArg (\s opts -> opts { optInput = Just s }) "INPUT_FILE")
        "input file"
    , Option ['o'] ["output"]
        (ReqArg (\s opts -> opts { optOutput = Just s }) "OUTPUT_FILE")
        "output file"
    , Option ['c'] ["count"]
        (ReqArg (\d opts -> opts { optCount = read d }) "VALUE")
        "cluster count"
    , Option ['p'] ["precision"]
        (ReqArg (\d opts -> opts { optCount = read d }) "VALUE")
        "precision"
    , Option ['s'] ["separator"]
        (ReqArg (\s opts -> opts { optSeparator = s }) "SEPARATOR")
        "cells separator"
    , Option ['m'] ["method"]
        (ReqArg (\c opts -> opts { optSeparator = c }) "NAME")
        "method name H or E"
    , Option ['h'] ["head"]
        (NoArg (\opts -> opts { optSkipHeadLine = True }))
        "skip head line"
    , Option ['f'] ["first"]
        (NoArg (\opts -> opts { optSkipFirstColumn = True }))
        "skip first column"
    , Option ['l'] ["last"]
        (NoArg (\opts -> opts { optSkipLastColumn = True }))
        "skip last column"
    , Option ['r'] ["rand-matrix"]
        (NoArg (\opts -> opts { optIsRandMatrix = True }))
        "init algorithm method: true - random generation of own, false - random generation of class centers."
    ]

parseArgs :: IO Options
parseArgs = do
    argv <- getArgs
    progName <- getProgName
    let header = "Usage: " ++ progName ++ " [OPTION...]"
    let helpMessage = usageInfo header options
    case getOpt RequireOrder options argv of
        (opts, [], []) -> return (foldl (flip id) defaultOptions opts)
        (_, _, errs) -> ioError (userError (concat errs ++ helpMessage))

main :: IO ()
main = do
    options <- parseArgs
    putStrLn $ show options
    if optInput options == Nothing
    then putStrLn "There is no file to parse. Possibly you have missed -i argument. Try again please."
    else do 
        --contents <- fromFile $ fromJust $ optInput options
        --putStrLn contents
        --putStrLn $ show $ map (`getCells` ",") $ getRows contents
        --contents <- parseCSV $ parserOptions options
        --putStrLn $ show contents
        --putStrLn ""
        --randMatrix <- randMatrix (optCount options) (length contents)
        --putStrLn $ show randMatrix
        --putStrLn ""
        --putStrLn $ show $ randCenters (optCount options) contents
        
        --putStrLn ""
        --putStrLn $ show $ calculateClassCenters [[0,3,0],[1,5,0],[2,4,0]] [[0.1,0.9],[0.2,0.8],[0.3,0.7]] 
        
        --putStrLn ""
        --putStrLn $ show $ calculateMembershipMatrix euclideanDistance [[0,3,0],[1,5,0],[2,4,0]] [[0.10333333333333332,0.25166666666666665,0.0],[0.4991666666666666,2.549583333333333,0.0]]
        
        --putStrLn ""
        --result <- defineClasses (fcmOptions options) contents
        --putStrLn $ show $ result
        
        writeResult options `catch` catcherror

catcherror :: IOError -> IO()
catcherror ex = ioError ex

writeResult :: Options -> IO()
writeResult options 
    | isNothing (optOutput options) = do
        result <- doStuff options
        putStrLn $ prettyMatrix result
    | otherwise = do
        result <- doStuff options
        writeFile (fromJust (optOutput options)) (prettyMatrix result)

prettyMatrix :: [[Double]] -> String
prettyMatrix [] =  ""
prettyMatrix (h:m) = show h ++ "\n" ++ prettyMatrix m

doStuff :: Options -> IO [[Double]]
doStuff options = do
    contents <- parseCSV $ parserOptions options
    result <- defineClasses (fcmOptions options) contents
    return result

parserOptions :: Options -> ParserOptions
parserOptions options = ParserOptions {
    fileName            = fromJust $ optInput options
    , separator         = optSeparator options
    , skipHeadLine      = optSkipHeadLine options
    , skipFirstColumn   = optSkipFirstColumn options
    , skipLastColumn    = optSkipLastColumn options
}

fcmOptions :: Options -> FCMOptions
fcmOptions options = FCMOptions {
    clusterCountOpt     = optCount options
    , precisionOpt      = optPrecision options
    , methodOpt         = optMethod options
    , isRandMatrixOpt   = optIsRandMatrix options
}