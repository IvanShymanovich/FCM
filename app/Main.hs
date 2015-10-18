module Main (main) where

import System.Console.GetOpt
import System.Environment(getArgs, getProgName)
import Data.Maybe ( fromMaybe )

data Options = Options {
    optInput                :: Maybe FilePath
    , optOutput             :: Maybe FilePath
    , optSeparator          :: String
    , optSkipHeadLine       :: Bool
    , optSkipFirstColumn    :: Bool
    , optSkipLastColumn     :: Bool
} deriving Show

defaultOptions = Options {
    optInput                = Nothing
    , optOutput             = Nothing
    , optSeparator          = ","
    , optSkipHeadLine       = False
    , optSkipFirstColumn    = False
    , optSkipLastColumn     = False
}

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['i'] ["input"]
        (ReqArg (\s opts -> opts { optInput = Just s }) "INPUT")
        "input file"
    , Option ['o'] ["output"]
        (ReqArg (\s opts -> opts { optOutput = Just s }) "OUTPUT")
        "output file"
    , Option ['s'] ["separator"]
        (ReqArg (\s opts -> opts { optSeparator = s }) "SEPARATOR")
        "cells separator"
    , Option ['h'] ["head"]
        (NoArg (\opts -> opts { optSkipHeadLine = True }))
        "skip head line"
    , Option ['f'] ["first"]
        (NoArg (\opts -> opts { optSkipFirstColumn = True }))
        "skip first column"
    , Option ['l'] ["last"]
        (NoArg (\opts -> opts { optSkipLastColumn = True }))
        "skip last column"
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
