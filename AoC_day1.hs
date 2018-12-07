import System.Environment
import Text.Read 


parseItem :: String -> Maybe Int
parseItem = readMaybe . filter (/= '+')

parseList :: [String] -> Maybe [Int]
parseList = traverse parseItem 

partA :: [String] -> Maybe Int
partA = fmap sum . parseList

partB :: [String] -> Maybe Int
partB xs = parseList xs >>= compute 
    where 
        compute :: [Int] -> Maybe Int
        compute xs = (findDup [] . scanl (+) 0 . cycle) xs 
        findDup _ [] = Nothing
        findDup seen (x:xs) = if x `elem` seen then Just x else findDup (x:seen) xs
main = do
    content <- readFile "day1input.txt"
    let resultA = partA $ lines content
    print resultA
    let resultB = partB $ lines content
    print resultB
