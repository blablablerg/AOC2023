{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

import           Control.Lens
import           Control.Lens.TH
import           Data.Bifunctor
import           Data.Char
import           Data.List
import           Data.List.Split
import qualified Data.Map.Strict    as M
import           Data.Maybe
import           Data.Ord
import qualified Data.Set           as S
import           Debug.Trace
import           Text.Pretty.Simple

data Point = P {_x :: Int, _y :: Int} deriving (Eq, Show, Ord)
makeLenses ''Point

type Platform = M.Map Point Char
data Direction = North | East | West | South

main :: IO ()
main = do
  inputdata <- lines <$> readFile "input"
  let coordinates = [P x y | y <- [1..length inputdata], x <- [1..length (head inputdata)]]
  let platform = M.fromList $ zip coordinates (concat inputdata)
  let part1 =  calcLoad $ tiltLever platform North
  let cycles = 1000000000
  let (start, looplen) = findCycle platform
  let part2 = calcLoad $ tiltLever2 platform (((cycles - start) `mod` looplen) + start)
  print $ part2

findCycle :: Platform -> (Int, Int)
findCycle plf = fc [plf] plf
  where
    fc store plf =
      let plf' = tiltLever2 plf 1
                   in case elemIndex plf' store of
                        Nothing -> fc (plf':store) plf'
                        Just i  -> (length store - i, i + 1)

tiltLever2 :: Platform -> Int -> Platform
tiltLever2 plf n = foldl' tiltLever plf (take (4 * n)
                                         (cycle [North, West, South, East]))

tiltLever :: Platform -> Direction -> Platform
tiltLever plf d = tl (sortBy (pointsortd d) $ M.keys plf) plf d
  where
        tl [] plf d = plf
        tl (q:qs) plf d | plf M.! q == 'O' =
                          let end = findEnd plf q
                              plf' = swapPlaces plf q end
                          in  tl qs plf' d
                        | otherwise = tl qs plf d

        findEnd plf pos | M.findWithDefault '#' (direction d pos) plf == '.'
                                             = findEnd plf (direction d pos)
                        | otherwise = pos

        swapPlaces plf a b = let va = plf M.! a
                                 vb = plf M.! b
                             in M.insert b va $ M.insert a vb plf

direction :: Direction -> Point -> Point
direction North  pos = pos & y -~ 1
direction West  pos  = pos & x -~ 1
direction South pos  = pos & y +~ 1
direction East pos   = pos & x +~ 1

pointsortd :: Direction -> Point -> Point -> Ordering
pointsortd North = comparing _y <> comparing _x
pointsortd West  = comparing _y <> comparing _x
pointsortd South = comparing (Down . _y) <> comparing _x
pointsortd East  = comparing _y <> comparing (Down . _x)

calcLoad :: M.Map Point Char -> Int
calcLoad plf = fst $ foldr ((\x (acc,m) -> (x * m + acc , m + 1)) . length
               . filter ((== 'O') . snd)) (0,1) (groupBy (\(p1,_) (p2, _) -> p1._y == p2._y)
               $ sortBy pointmapsort (M.toList plf))

printMap :: Platform -> IO ()
printMap m = do
  let lines = map (map snd) . groupBy (\(p1,_) (p2, _) -> p1._y == p2._y)
              $ sortBy pointmapsort (M.toList m)
  printGrid lines

pointmapsort :: (Point, b) -> (Point, b) -> Ordering
pointmapsort = comparing (_y . fst) <> comparing (_x . fst)

pointsort :: Point -> Point -> Ordering
pointsort = comparing _y <> comparing _x

printGrid :: [String] -> IO ()
printGrid = putStrLn . unlines
