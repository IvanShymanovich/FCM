module FCM.FCM
    (
    randMatrix, randCenters, calculateClassCenters, hammingDistance, euclideanDistance, calculateMembershipMatrix, defineClasses, FCMOptions(..)
    ) where
import Data.List
import Data.List.Split
import System.Random

data FCMOptions = FCMOptions {
    clusterCountOpt            :: Int
    , precisionOpt      :: Double
    , methodOpt         :: Char
    , isRandMatrixOpt   :: Bool
} deriving Show

type DistanceFunc = ([Double] -> [Double] -> Double)

defineClasses :: FCMOptions -> [[Double]] -> IO [[Double]]
defineClasses options source = do
    matrix <- generateInitialMatrix options source
    return $ solveWhileAccuracy source matrix (precisionOpt options) (choseDistanceFunc options)

generateInitialMatrix :: FCMOptions -> [[Double]] -> IO [[Double]]
generateInitialMatrix options source =
    if (isRandMatrixOpt options)
        then randMatrix (clusterCountOpt options) (length source)
        else return $ calculateMembershipMatrix distFunc source centerClusters 
            where centerClusters = randCenters (clusterCountOpt options) source
                  distFunc = choseDistanceFunc options

choseDistanceFunc :: FCMOptions -> DistanceFunc
choseDistanceFunc options = 
    case methodOpt options of 
        'H' -> hammingDistance
        _ -> euclideanDistance

solveWhileAccuracy :: [[Double]] -> [[Double]] -> Double -> DistanceFunc -> [[Double]]
solveWhileAccuracy source membershipMatrix e f = 
    if (calculateMatrixDiff membershipMatrix newMembershipMatrix < e )
        then newMembershipMatrix
        else solveWhileAccuracy source newMembershipMatrix e f
             where newMembershipMatrix = calculateMembershipMatrix f source (calculateClassCenters source membershipMatrix) 

calculateMatrixDiff :: [[Double]] -> [[Double]] -> Double
calculateMatrixDiff x y = maximum rowMax
    where
        diffs = zipWith(\xRow yRow -> rem xRow yRow) x y
        rem xRow yRow = zipWith(\xEl yEl -> abs $ xEl - yEl) xRow yRow
        rowMax = map(\row -> getMax row) diffs

getMax :: [Double] -> Double
getMax [] = 0
getMax arr = maximum arr

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

calculateClassCenters :: [[Double]] -> [[Double]] -> [[Double]]
calculateClassCenters vectors own = map calculateCenter $ transpose own
    where
        calculateCenter ownVector = map (/ ( sum ownVector )) $ numerator ownVector
        numerator u = map sum $ transpose $ zipWith (\ a b -> map (a^(length $ head vectors)*) b) u vectors -- remove r

calculateMembershipMatrix :: DistanceFunc -> [[Double]] -> [[Double]] -> [[Double]]
calculateMembershipMatrix f vectors centers = map calculateVectorMembership vectors
    where
        calculateVectorMembership vector = map (calculate vector) centers
        calculate vector centerK = 1 / (sum $ map (\ centerJ -> ((f vector centerK) / (f vector centerJ))^2) centers)
