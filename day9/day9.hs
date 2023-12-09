{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

import           Control.Applicative
import           Control.Lens
import           Control.Lens.TH
import qualified Data.Attoparsec.Text as P
import           Data.Either          (rights)
import           Data.List
import qualified Data.Map.Strict      as M
import           Data.Maybe
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Debug.Trace
import           Text.Pretty.Simple


main = do
  inputdata <- T.lines <$> T.readFile "input"
  let report = rights $ P.parseOnly (P.many1 (P.signed P.decimal <* P.skipSpace)) <$> inputdata
  let part1 = sum $ map (last . head . addSums . rptDifferences) report
  let part2 = sum $ map (head . head . addSums2 . rptDifferences) report
  print $ part2

addSums2 :: [[Int]] -> [[Int]]
addSums2 = addSums2' []
  where
    addSums2' r [] = r
    addSums2' r (l:ls) | all (== 0) l = addSums2' ((0:l):r) ls
    addSums2' (r:rs) (l:ls) = let l' = (head l - head r):l
                              in addSums2' (l':r:rs) ls

addSums :: [[Int]] -> [[Int]]
addSums = addSums' []
  where
    addSums' r [] = r
    addSums' r (l:ls) | all (== 0) l = addSums' ((0:l):r) ls
    addSums' (r:rs) (l:ls) = let l' = l ++ [last r + last l]
                             in addSums' (l':r:rs) ls

rptDifferences :: [Int] -> [[Int]]
rptDifferences xs = reverse (xs : unfoldr diffs xs)
  where
    diffs xs | all (== 0) xs = Nothing
             | otherwise     = Just (differences xs , differences xs)

differences :: [Int] -> [Int]
differences xs = zipWith (-) (tail xs) (init xs)


