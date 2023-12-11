{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

import           Control.Lens
import           Control.Lens.TH
import           Data.Char
import           Data.List
import qualified Data.Map.Strict    as M
import           Data.Maybe
import           Debug.Trace
import           Text.Pretty.Simple

data Point = P {_x :: Int, _y :: Int} deriving (Eq, Show, Ord)
makeLenses ''Point

main :: IO ()
main = do
  inputdata <- transpose . expand . transpose . expand . lines <$> readFile "input"
  let coordinates = [P x y | y <- [1..length inputdata], x <- [1..length (head inputdata)]]
  let starMap = M.fromList $ zip coordinates (concat inputdata)
  let filteredStM = M.keys .  M.filter (== '#') $ starMap
  let galaxies = zip filteredStM [1 .. length filteredStM]
  let paired_glxs = [(x,y) | x <- galaxies, y <- galaxies, snd x < snd y]
  let part1 =  sum $ map (uncurry calcSteps) $ paired_glxs & traversed . each %~ fst
  let hLines = getElines inputdata
  let vLines = getElines . transpose $ inputdata
  let weight = 1_000_000
  let part2 =  sum $ map (uncurry (calcSteps2 (hLines, vLines) weight))
                   $ paired_glxs & traversed . each %~ fst
  print $ part2

calcSteps2 :: ([Int], [Int]) -> Int -> Point -> Point -> Int
calcSteps2 (hl, vl) w p1@(P x1 y1) p2@(P x2 y2) =
  let hls = length $ hl `intersect` range y1 y2
      vls = length $ vl `intersect` range x1 x2
      steps = calcSteps p1 p2
  in steps - (hls + vls) + w * (hls + vls)

  where
   range x y | y >= x = [x .. y]
             | otherwise = [y .. x]

getElines :: [String] -> [Int]
getElines = ge [] 1
 where
   ge :: [Int] -> Int -> [String] -> [Int]
   ge ns _ [] = ns
   ge ns n (l:ls) | all (== '+') l = ge (n:ns) (n+1) ls
                  | otherwise      = ge ns   (n+1) ls 

calcSteps :: Point -> Point -> Int
calcSteps (P x1 y1) (P x2 y2) = dist x1 x2 + dist y1 y2

dist :: Num a => a -> a -> a
dist x y = abs (x - y)

expand :: [String] -> [String]
expand []                      = []
expand (l:ls) | all (`elem` ['.','+']) l = replace l:expand ls
              | otherwise      = l:expand ls
  where
    replace l = replicate (length l) '+'

printGrid :: [String] -> IO ()
printGrid = mapM_ putStrLn


