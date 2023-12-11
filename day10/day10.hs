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
import           Debug.Trace
import           Text.Pretty.Simple

data Point = Point {_x :: Int, _y :: Int} deriving (Eq, Show)
makeLenses ''Point

instance Ord Point where
  compare :: Point -> Point -> Ordering
  compare  = comparing _y <> comparing _x

type PipeMap = M.Map Point Char
type PipeList = [(Point, Char)]

main :: IO ()
main = do
  inputdata <- lines <$> readFile "input"
  let coordinates = [Point x y | y <- [1..length inputdata], x <- [1..length (head inputdata)]]
  let pipeMap = M.fromList $ zip coordinates (concat inputdata)
  let bigloop= head $ traversePipes pipeMap (getStart pipeMap)
  let part1 =  flip (-) 1 . maximum $ zipWith min [1 .. length bigloop] (reverse [1 .. length bigloop])
  let filledMap2 = fillMap . M.map (const '.') . M.mapKeys doublePoint $ pipeMap
  let bigloop2 = connectLoopPoints $ map doublePoint bigloop
  let bigloop2pts = toPlusPoints bigloop2
  let pipeMap2 = foldr (\(p,c) m -> M.insert p c m) filledMap2 bigloop2pts
  let dots = M.keys $ M.filter (== '.') pipeMap2
  let commasAndDots = M.filter (`elem` ['.',',']) pipeMap2
  let (succs,fails) = floodPipes pipeMap2 (M.keys commasAndDots)
  let part2 = length (dots `intersect` fails)
  print $ part2

floodPipes :: PipeMap -> [Point] -> ([Point], [Point])
floodPipes pm = fp [] ([],[])
  where
    fp :: [Point] -> ([Point], [Point]) -> [Point] -> ([Point], [Point])
    fp prevdots results [] = results
    fp prevdots (succs, fails) (dot:dots) | dot `elem` prevdots = fp prevdots (succs, fails) dots
                                          | otherwise =
       let (test, area) = flood dot
       in fp (area ++ prevdots)
             (if test then (area ++ succs, fails) else (succs, area ++ fails))
          dots
    flood :: Point -> (Bool, [Point])
    flood d = fl False [] [d]

    fl :: Bool -> [Point] -> [Point] -> (Bool, [Point])
    fl ob          v       []        = (ob, v)
    fl oldboundary visited (q:queue) =
      let nexts = filter (\(pt, c) -> c /= '+')
                  . map (\np -> (np, M.findWithDefault 'B' np pm))
                  $ filter (`notElem` visited) (getAdjs2 q)
          boundary = any ((== 'B') . snd) ((q, (M.!) pm q):nexts)
          nextpts = map fst . filter (\(pt,c) -> c /= 'B') $ nexts
       in if boundary
          then fl boundary (q:visited) (nextpts ++ queue)
          else fl oldboundary (q:visited) (nextpts ++ queue)

fillMap :: PipeMap -> PipeMap
fillMap m = let pts = M.keys m
                pts' = connectPoints fillPoints pts \\ pts
               in foldr ((\(p,c) m' -> M.insert p c m') . (, ',')) m pts'

toPlusPoints :: [Point] -> [(Point, Char)]
toPlusPoints = map (, '+')

doublePoint :: Point -> Point
doublePoint (Point x y) = Point (2*x-1) (2 * y - 1)

connectLoopPoints :: [Point] -> [Point]
connectLoopPoints = connectPoints connectLoop

connectPoints :: (Point -> Point -> Maybe [Point]) -> [Point] -> [Point]
connectPoints f [] = []
connectPoints f [x] = [x]
connectPoints f (x:y:xs) = case f x y of
                             Just x' -> (x:x') ++ connectPoints f (y:xs)
                             Nothing -> x:connectPoints f (y:xs)

connectLoop :: Point -> Point -> Maybe [Point]
connectLoop p1 p2 | abs (p1._x - p2._x) == 2
                    , p1._y == p2._y = let x' = min p1._x p2._x + 1
                                       in Just [Point x' p1._y]
connectLoop p1 p2 | abs (p1._y - p2._y) == 2
                    , p1._x == p2._x = let y' = min p1._y p2._y + 1
                                       in Just [Point p1._x y']
                    | otherwise      = Nothing

fillPoints :: Point -> Point -> Maybe [Point]
fillPoints p1 p2 | abs (p1._x - p2._x) == 2
                    , p1._y == p2._y = let x' = min p1._x p2._x + 1
                                       in Just [Point x' p1._y]
fillPoints p1 p2 | abs (p1._y - p2._y) == 2
                    -- , p2._x == 1
                                 = let y' = min p1._y p2._y + 1
                                       xs' = [p2._x .. p1._x]
                                       in Just $ map (`Point` y') xs'
                    | otherwise      = Nothing

printGrid :: [(Point, Char)] -> IO ()
printGrid lmap = do
  let lines = map (map snd) . groupBy (\(p1,_) (p2, _) -> p1._y == p2._y) $ sortOn fst lmap
  mapM_ putStrLn lines

getAdjs2 :: Point -> [Point]
getAdjs2 p = [p & x +~ 1, p & x -~ 1, p & y +~ 1, p & y -~ 1]

traversePipes :: PipeMap -> Point -> [[Point]]
traversePipes pm start = tp [] start pm [[start]]
  where
    tp :: [[Point]] -> Point -> PipeMap -> [[Point]] -> [[Point]]
    tp rs start pm []           = rs
    tp rs start pm (path@(p:ps):paths) =
      let prev = take 1 ps
          pds = getNext pm p \\ prev
          pPaths' = map (: path) pds
          hits = filter (\path' -> head path' == start) pPaths'
          paths' = filter (\path' -> head path' /= start) pPaths'
          in --trace ("paths: " ++ show  (map length (paths' ++ paths))) $
          tp (hits ++ rs) start pm (paths' ++ paths)


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

