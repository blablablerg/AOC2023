import qualified Data.Attoparsec.Text as Text
import           Data.Char
import           Data.Either
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as Text
import qualified Data.Text.IO         as Text
import           Text.Regex.Posix     as Text

type TextNumberMap = Map.Map String Int

main :: IO ()
main = do
    input <- map Text.unpack . Text.lines <$> Text.readFile "input"
    print $ part2 input

part2 input =
    sum
        $ zipWith (\x y -> x * 10 + y) (getFirstNumbers input) (getLastNumbers input)

getFirstNumbers :: [String] -> [Int]
getFirstNumbers = map getFirstNumber

getFirstNumber :: String -> Int
getFirstNumber = getNumber textNumbers

getLastNumbers :: [String] -> [Int]
getLastNumbers = map getLastNumber

getLastNumber :: String -> Int
getLastNumber s = getNumber textRevNumbers (reverse s)

getNumber :: TextNumberMap -> String -> Int
getNumber m [] = error "empty string or no number"
getNumber m (x : xs) | isDigit x = digitToInt x
getNumber m xs = case matchTextNumber m xs of
    Just i  -> i
    Nothing -> getNumber m (tail xs)

matchTextNumber :: TextNumberMap -> String -> Maybe Int
matchTextNumber m str | Map.null m = Nothing
matchTextNumber m str =
    let numbertext = head $ Map.keys m
     in case str =~ ("^" ++ numbertext) :: String of
            [] -> matchTextNumber (Map.delete numbertext m) str
            xs -> Just $ m Map.! xs

textNumbers :: TextNumberMap
textNumbers =
    Map.fromList
        $ zip
            ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
            [1 .. 9]

textRevNumbers :: TextNumberMap
textRevNumbers = Map.mapKeys reverse textNumbers
