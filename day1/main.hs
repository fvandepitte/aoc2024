import System.IO
import Data.List
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let filePath = head args
    contents <- readFile filePath
    putStrLn "Part1"
    print $ solve contents 
    putStrLn "Part2"
    print $ solve2 contents

parseFile :: String -> [[Int]]
parseFile = map (map read . words) . lines

calculateDistance :: [[Int]] -> [Int]
calculateDistance [a, b] = map abs $ zipWith (-) a b

solve :: String -> Int 
solve = sum . calculateDistance . map sort . transpose . parseFile

calculateSimularity :: [[Int]] -> [Int]
calculateSimularity [a, b] = map (calculateSimularityScore a) b

calculateSimularityScore :: [Int] -> Int -> Int
calculateSimularityScore bs a = (*) a $ length $ filter (==a) bs

solve2 :: String -> Int
solve2 = sum . calculateSimularity . map sort . transpose . parseFile