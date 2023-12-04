import           Data.Char
import           Data.List
import           Data.List.Split
import qualified Data.Map.Strict    as M
import           Data.Maybe         (fromJust)
import           Data.Ord
import           Debug.Trace
import           Text.Pretty.Simple

data Point = Point {_x :: Int, _y :: Int} deriving (Eq, Show)
type PartNumber = [(Point, Char)]

instance Ord Point where
  compare :: Point -> Point -> Ordering
  compare  = comparing _y <> comparing _x

main = do
  inputdata <- lines <$> readFile "input"
  let coordinates = [Point x y | y <- [1..length inputdata], x <- [1..length (head inputdata)]]
  let engineSchematic = M.fromList $ zip coordinates (concat inputdata)
  let numbers =  M.toList $ M.filter isDigit engineSchematic
  let adjNumbers = splitAdj (\(Point x1 y1, v1) (Point x2 y2, v2) -> x1 + 1 == x2 && y1 == y2) numbers
  let enginePartNumbers = filter (isEnginepart engineSchematic) adjNumbers
  let part1 = sum $ map ((\n -> read n :: Int) . map snd) enginePartNumbers
  let stars = concatMap (findStars engineSchematic) adjNumbers
  let gears = findGears stars
  let part2 = sum $ map (product . map ((\n -> read n :: Int) . map snd)) gears
  print $ part2

findGears :: [(PartNumber, Point)] -> [[PartNumber]]
findGears starlist = let sorted_starlist = sortOn snd starlist
                         potGears = map (map fst) $ groupBy (\(pn1,s1) (pn2, s2) -> s1 == s2) sorted_starlist
                         gears = filter (\l -> length l == 2) potGears
                         in gears

findStars :: M.Map Point Char -> [(Point, Char)] -> [(PartNumber, Point)]
findStars m ps = let nbs = neighboursAdj (map fst ps)
                     nbs_symbols = map (\p -> (p, M.findWithDefault '.' p m)) nbs
                     stars =  map (\(p,c) -> (ps,p)) (filter (\(p,c) -> c == '*') nbs_symbols)
                 in stars

isEnginepart :: M.Map Point Char -> PartNumber -> Bool
isEnginepart m ps = let nbs = neighboursAdj (map fst ps)
                        nbs_symbols = map (\p -> M.findWithDefault '.' p m) nbs
                    in not $ all (== '.') nbs_symbols

neighboursAdj :: [Point] -> [Point]
neighboursAdj ps = let ns = nub $ concatMap neighbours ps
                   in ns \\ ps

neighbours :: Point -> [Point]
neighbours (Point x y) = [Point dx dy | dy <- [y-1..y+1], dx <- [x-1..x+1], (dx,dy) /= (x,y)]

-- Heel leuk dit, maar je had splitWhen kunnen gebruiken uit Data.List.Split
splitAdj :: (a -> a -> Bool) -> [a] -> [[a]]
splitAdj f = unfoldr (splitAdjOnce f)

splitAdjOnce :: (a -> a -> Bool) -> [a] -> Maybe ([a],[a])
splitAdjOnce = splitAdjOnce' []
 where
   splitAdjOnce' :: [a] -> (a -> a -> Bool) -> [a] -> Maybe ([a], [a])
   splitAdjOnce' [] f [] = Nothing
   splitAdjOnce' result f [] = Just (reverse result, [])
   splitAdjOnce' [] f (x:xs) = splitAdjOnce' [x] f xs
   splitAdjOnce' (r:rs) f  (x:xs) | f r x = splitAdjOnce' (x:r:rs) f xs
   splitAdjOnce' result f l = Just (reverse result, l)

