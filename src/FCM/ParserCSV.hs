module FCM.ParserCSV
    ( getRows, ParserOptions(..), parseCSV, getCells
    ) where

import  qualified   Data.ByteString.Lazy    as BL
import  qualified   Data.Vector             as V
import              System.IO
import              Data.List.Split

data ParserOptions = ParserOptions {
    fileName            :: FilePath
    , separator         :: String
    , skipHeadLine      :: Bool
    , skipFirstColumn   :: Bool
    , skipLastColumn    :: Bool
} deriving Show


parseCSV :: ParserOptions -> IO [[Double]]
parseCSV options = do
    content <- readFile $ fileName options
    let preparedCells = map (`getCells` options) $ getRows content $ skipHeadLine options
    return $ map ( map read ) preparedCells

getRows :: String -> Bool ->[String]
getRows content True = tail $ lines content
getRows content _ = lines content

getCells :: String -> ParserOptions -> [String]
getCells row (ParserOptions _ separator _ True False) = tail $ splitOn separator row
getCells row (ParserOptions _ separator _ False True) = init $ splitOn separator row
getCells row (ParserOptions _ separator _ True True) = tail $ init $ splitOn separator row
getCells row (ParserOptions _ separator _ _ _) = splitOn separator row
