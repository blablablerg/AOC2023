{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

import           Control.Applicative
import           Control.Lens
import           Control.Lens.TH
import qualified Data.Attoparsec.Text       as P
import           Data.Either
import           Data.List
import qualified Data.Map.Strict            as M
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Debug.Trace                ()
import           Text.Pretty.Simple

data Amap = Amap {_source :: Int, _dest :: Int, _range :: Int} deriving (Show)
makeLenses ''Amap

data Amap' = Amap' {_source_range :: (Int, Int), _jump :: Int} deriving (Show)
makeLenses = ''Amap'

type Range = (Int, Int)

main = do
  inputdata <- T.readFile "input"
  let (Right (seeds, listofAmaps)) = P.parseOnly parseAlmanac inputdata
  let listofAmaps' = listofAmaps & traversed . each %~ mkAmap'
  let converters = map convert listofAmaps'
  let part1 = minimum $ map (\seed -> foldl' (&) seed converters) seeds
  let seeds2 = map (: []) $ getSeeds2 seeds
  let converters2 = map convert2 listofAmaps'
  let converted2 = map (\seedrange -> foldl' (&) seedrange converters2) seeds2
  let part2 = minimum $ converted2 ^.. folded . folded . each
  print $ part2

convert2 :: [Amap'] -> [Range] -> [Range]
convert2 = convert2' []
  where
    convert2' :: [Range] -> [Amap'] -> [Range] -> [Range]
    convert2' conv [] rs = rs ++ conv
    convert2' conv (a:as) rs = let (c, nc) = convertRanges a rs
                                             in convert2' (c ++ conv) as nc

convertRanges :: Amap' -> [Range] -> ([Range], [Range])
convertRanges = convertRanges' ([],[])
  where
    convertRanges' :: ([Range], [Range]) -> Amap' -> [Range] -> ([Range], [Range])
    convertRanges' result amap [] = result
    convertRanges' (conv, notconv) amap (r : rs) =
      case convertRange amap r of
        (Nothing, nc) -> convertRanges' (conv, nc ++ notconv) amap rs
        (Just c', nc) -> convertRanges' (c' : conv, nc ++ notconv) amap rs

convertRange :: Amap' -> Range -> (Maybe Range, [Range])
convertRange (Amap' x j) y = case getIandD y x of
                               (Nothing, rest) -> (Nothing, rest)
                               (Just (y'1, y'2), rest) -> (Just (y'1 +j, y'2 + j), rest)

convert :: [Amap'] -> Int -> Int
convert []                   i             = i
convert ((Amap' r j):as) i | i `inRange` r = i + j
convert (a:as)               i             = convert as i

inRange :: Int -> (Int, Int) -> Bool
inRange i (lb,ub) = i >= lb && i <= ub

getIandD :: Range -> Range -> (Maybe Range, [Range])
getIandD (x1, x2) (y1, y2) | x1 > y2 || x2 < y1 = (Nothing, [(x1, x2)])
getIandD (x1, x2) (y1, y2) | x1 < y1 && x2 > y2 = (Just (y1, y2), [(x1, y1 - 1), (y2 + 1, x2)])
getIandD (x1, x2) (y1, y2) | x1 >= y1 && x2 <= y2 = (Just (x1, x2), [])
getIandD (x1, x2) (y1, y2) | x1 >= y1 && x2 > y2 = (Just (x1, y2), [(y2 + 1, x2)])
getIandD (x1, x2) (y1, y2) | x1 < y1 && x2 <= y2 = (Just (y1, x2), [(x1, y1 - 1)])

getSeeds2 :: [Int] -> [(Int, Int)]
getSeeds2 s = let s' = zip (s ^.. itraversed . indices even) (s ^.. itraversed . indices odd)
              in map (\(start, range) -> (start, start + range - 1))  s'

mkAmap' :: Amap -> Amap'
mkAmap' a = Amap' (a._source, a._source + a._range - 1) (a._dest - a._source)

parseAlmanac :: P.Parser ([Int], [[Amap]])
parseAlmanac = do
  "seeds:" >> P.skipSpace
  seeds <- P.many1 (P.decimal <* P.skipSpace)
  listofAmaps <- P.many1 parseAmaps
  return (seeds, listofAmaps)

parseAmaps :: P.Parser [Amap]
parseAmaps = do
  P.skipWhile (not . P.isEndOfLine) >> P.endOfLine
  amaps <- P.many1 parseAmap
  P.skipSpace
  return amaps

parseAmap :: P.Parser Amap
parseAmap = do
  dest <- P.decimal
  P.skipSpace
  source <- P.decimal
  P.skipSpace
  range <- P.decimal
  P.skipSpace
  return $ Amap source dest range
