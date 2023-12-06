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

main = do
  inputdata <- T.readFile "input"
  let (Right (times, records)) = P.parseOnly parseRaces inputdata
  let pTimes = zip times $ map (\t -> [0 .. t]) times
  let pDistances = zip records $ map (uncurry distances) pTimes
  let part1 = product $ map (length . (\(r, prs) -> filter (> r) prs)) pDistances
  let (time2, record2) = (toPart2 times, toPart2 records)
  let part2 = (\(x1, x2) ->  x2 - x1 + 1) $ abc time2 record2
  print $ part2

abc :: Int -> Int -> (Int, Int)
abc t d = let t' = fromIntegral t
              d' = fromIntegral d
              b  = t'/2
              ds  = sqrt (t' * t' - 4 * d') / 2
              r = [b + ds, b - ds]
          in ((ceiling . minimum) r, (floor . maximum) r)

toPart2 :: [Int] -> Int
toPart2 xs = read (concatMap show xs) :: Int

distances :: Int -> [Int] -> [Int]
distances time = map (distance time)

distance :: Int -> Int -> Int
distance time button = -(button * button) + (button * time)

parseRaces :: P.Parser ([Int], [Int])
parseRaces = do
  "Time:" >> P.skipSpace
  times <- P.many1 (P.decimal <* P.skipSpace)
  "Distance:" >> P.skipSpace
  records <- P.many1 (P.decimal <* P.skipSpace)
  return (times, records)
