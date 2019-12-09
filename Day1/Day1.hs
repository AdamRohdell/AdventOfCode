import Data.List
import Numeric.Extra

readModules :: FilePath -> IO Int
readModules p = do
            ls <- readFile p
            let ls' = lines ls
            return $ calculateFuel ls'


calculateFuel :: [String] -> Int
calculateFuel = foldr (\m -> (+) (calculateFuelOfFuel (read m))) 0

calculateFuelOfFuel :: Int -> Int
calculateFuelOfFuel fuel 
                    | x < 0 = 0
                    | otherwise = calculateFuelOfFuel x + x
                            where x =  (div fuel 3) -2

--    Solution for the first star
--calculateFuel :: [String] -> Int
--calculateFuel = foldr (\m -> (+) (floor (read m)/3) -2) 0
