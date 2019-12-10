import Data.List
import qualified Data.Text as T
import Data.Maybe

readInput :: FilePath -> IO Int
readInput p = do
            intsString <- readFile p
            let ints = T.split (==',') (T.pack intsString)
                xs   = map (read . T.unpack) ints
            return $ head (readOpCodes xs)



readOpCodes :: [Int] -> [Int]
readOpCodes []      = []
readOpCodes (99:is) = 99:is
readOpCodes ints = readOpCode n ++ readOpCodes values'

    where
            n = head ints
            values' = drop 4 ints
            readOpCode:: Int -> [Int]
            readOpCode n  
                    | n == 1  = addValues (splitAt (ints !! (getValues n-1)) ints)
                    | n == 2  = mulValues (splitAt (ints !! (getValues n-1)) ints)

            addValues :: ([Int],[Int]) -> [Int]
            addValues (x,[])                             = x
            addValues (junk, first:second:output:values) = junk++first:second:(first + second):values

            mulValues :: ([Int],[Int]) -> [Int]
            mulValues (x,[])                             = x
            mulValues (junk, first:second:output:values) = junk++first:second:(first * second):values

            --AM NOT USING INDEXES, STOP USING VALUES

            getValues x 
                    | x + 3 <= length ints = fromJust (elemIndex x ints)+ 3
                    | otherwise            = error "end of list"

        


