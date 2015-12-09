module FCM.FCM
    ( 
    randMatrix, randCenters
    ) where

import Data.List.Split
import System.Random

data FCMOptions = FCMOptions {
    optCount            :: Int
    , optPrecision      :: Double
    , optMethod         :: Char
    , optIsRandMatrix   :: Bool
} deriving Show

--defineClasses :: FCMOptions -> [[Double]]
--defineClasses (FCMOptions clusterCount precision method True) = 

randMatrix :: Int -> Int -> IO [[Double]]
randMatrix clusterCount vectorCount = do
    row <- randRow $ clusterCount * vectorCount
    return $ map (\ row -> map (/ (sum row)) row) (chunksOf clusterCount row)

randRow :: Int -> IO [Double]
randRow length = do 
    stdGen <- newStdGen
    return $ take length (randoms stdGen :: [Double])

randCenters :: (Real a) => Int -> [[a]] -> [[a]]
randCenters clusterCount vectors = take clusterCount vectors

hammingDistance :: (Real a) => [a] -> [a] -> a
hammingDistance x y = sum . map (abs) $ zipWith (-) x y

euclideanDistance :: (Floating a) => [a] -> [a] -> a
euclideanDistance x y = sqrt . sum . map (^2) $ zipWith (-) x y

--calculateClassCenters :: (Real a) => [[a]] -> [[a]] -> [[a]]
--calculateClassCenters [] [] = 0
--calculateClassCenters (v:vectors) (u:own) = (sum (u**(length v)) v) / (sum )