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

data Point = Point {_x :: Int, _y :: Int} deriving (Eq, Show, Ord)
makeLenses ''Point

type PipeMap = M.Map Point Char
type PipeList = [(Point, Char)]

main :: IO ()
main = do
  inputdata <- lines <$> readFile "input"
  let coordinates = [Point x y | y <- [1..length inputdata], x <- [1..length (head inputdata)]]
  let pipeMap = M.fromList $ zip coordinates (concat inputdata)
  let bigloop= head $ traversePipes pipeMap (getStart pipeMap)
  let part1 =  flip (-) 1 . maximum $ zipWith min [1 .. length bigloop] (reverse [1 .. length bigloop])
  let doubleMap  = map doublePoint $ M.keys pipeMap
  let bigloop2 = S.fromList . connectLoopPoints . map doublePoint $ bigloop
  let dotkeys = S.difference (S.fromList doubleMap) bigloop2
  let commakeys = flip S.difference (S.union bigloop2 dotkeys) . S.fromList . connectPoints fillPoints $ doubleMap
  let commas = S.map (toPipe ',') commakeys
  let plusses = S.map (toPipe '+')  bigloop2
  let dots = S.map (toPipe '.') dotkeys
  let pipeMap2 = M.fromList . S.toList $ S.unions [commas, plusses, dots]
  let (succs,fails) = floodPipes pipeMap2 (S.toList (S.union commakeys dotkeys))
  let part2 = length (dotkeys `S.intersection` fails)
  print part2

floodPipes :: PipeMap -> [Point] -> (S.Set Point, S.Set Point)
floodPipes pm = fp S.empty (S.empty, S.empty)
  where
    fp :: S.Set Point -> (S.Set Point, S.Set Point) -> [Point] -> (S.Set Point, S.Set Point)
    fp prevdots results [] = results
    fp prevdots (succs, fails) (dot:dots) | dot `S.member` prevdots = fp prevdots (succs, fails) dots
                                          | otherwise =
       let (test, area) = flood dot
       in fp (S.union area prevdots)
             (if test then (S.union area succs, fails) else (succs, S.union area fails))
          dots
    flood :: Point -> (Bool, S.Set Point)
    flood d = fl False S.empty [d]

    fl :: Bool -> S.Set Point -> [Point] -> (Bool,S.Set Point)
    fl ob          v       []        = (ob, v)
    fl oldboundary visited (q:queue) =
      let nexts = filter (\(pt, c) -> c /= '+')
                  . map (\np -> (np, M.findWithDefault 'B' np pm))
                  $ filter (`S.notMember` visited) (getAdjs2 q)
          boundary = if not oldboundary
                     then any ((== 'B') . snd) ((q, (M.!) pm q):nexts)
                     else oldboundary
          nextpts = map fst . filter (\(pt,c) -> c /= 'B') $ nexts
       in fl boundary (S.insert q visited) (nextpts ++ queue)

toPipe :: Char -> Point -> (Point, Char)
toPipe c = (, c)

doublePoint :: Point -> Point
doublePoint (Point x y) = Point (2*x-1) (2 * y - 1)

connectLoopPoints :: [Point] -> [Point]
connectLoopPoints = connectPoints' connectLoop

connectPoints :: (Point -> Point -> Maybe [Point]) -> [Point] -> [Point]
connectPoints f pts = connectPoints' f $ sortBy (comparing _y <> comparing _x) pts

connectPoints' :: (Point -> Point -> Maybe [Point]) -> [Point] -> [Point]
connectPoints' f [] = []
connectPoints' f [x] = [x]
connectPoints' f (x:y:xs) = case f x y of
                             Just x' -> (x:x') ++ connectPoints' f (y:xs)
                             Nothing -> x:connectPoints' f (y:xs)

connectLoop :: Point -> Point -> Maybe [Point]
connectLoop p1 p2 | abs (p1._x - p2._x) == 2
                    , p1._y == p2._y = let x' = min p1._x p2._x + 1
                                       in Just [Point x' p1._y]
connectLoop p1 p2 | abs (p1._y - p2._y) == 2
                    , p1._x == p2._x = let y' = min p1._y p2._y + 1
                                       in Just [Point p1._x y']
                    | otherwise      = Nothing

fillPoints :: Point -> Point -> Maybe [Point]
fillPoints p1 p2 | p1._y == p2._y && p1._x /= p2._x = let xs' = range p1._x p2._x
                                                      in Just $ map (\x -> Point x p1._y) xs'
fillPoints p1 p2 | abs (p1._y - p2._y) == 2 = let y' = min p1._y p2._y + 1
                                                  xs' = [p2._x .. p1._x]
                                              in Just $ map (`Point` y') xs'
                 | otherwise   = Nothing

range:: Int -> Int -> [Int]
range x y | x <= y = [x + 1 .. y - 1]
          | otherwise = [y + 1 .. x - 1]

printGrid :: [(Point, Char)] -> IO ()
printGrid lmap = do
  let lines = map (map snd) . groupBy (\(p1,_) (p2, _) -> p1._y == p2._y) $ sortBy cmpLmap lmap
  putStrLn $ unlines lines

cmpLmap :: (Point, b) -> (Point, b) -> Ordering
cmpLmap = comparing (_y . fst) <> comparing (_x . fst)

getAdjs2 :: Point -> [Point]
getAdjs2 p = [p & x +~ 1, p & x -~ 1, p & y +~ 1, p & y -~ 1]

traversePipes :: PipeMap -> Point -> [[Point]]
traversePipes pm start = tp [] start [[start]]
  where
    tp :: [[Point]] -> Point -> [[Point]] -> [[Point]]
    tp rs start []           = rs
    tp rs start (path@(p:ps):paths) =
      let prev = take 1 ps
          pds = getNext pm p \\ prev
          pPaths' = map (: path) pds
          hits = filter (\path' -> head path' == start) pPaths'
          paths' = filter (\path' -> head path' /= start) pPaths'
       in tp (hits ++ rs) start (paths' ++ paths)

getNext :: PipeMap -> Point -> [Point]
getNext pm p = let ds = getAdjs pm p
               in filter (\d -> p `elem` getAdjs pm d) ds

getStart :: PipeMap -> Point
getStart =  head . M.keys . M.filter (== 'S')

getAdjs :: PipeMap -> Point -> [Point]
getAdjs pm p = getAdjf (M.findWithDefault '.' p pm) p

getAdjf :: Char -> (Point -> [Point])
getAdjf '.' = (: [])
getAdjf '|' = \p -> [p & y +~ 1, p & y -~ 1]
getAdjf '-' = \p -> [p & x +~ 1, p & x -~ 1]
getAdjf 'L' = \p -> [p & x +~ 1, p & y -~ 1]
getAdjf 'J' = \p -> [p & x -~ 1, p & y -~ 1]
getAdjf '7' = \p -> [p & x -~ 1, p & y +~ 1]
getAdjf 'F' = \p -> [p & x +~ 1, p & y +~ 1]
getAdjf 'S' = \p -> [p & x +~ 1, p & x -~ 1, p & y +~ 1, p & y -~ 1]

