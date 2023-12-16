{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

import           Control.Applicative
import           Control.Lens
import           Control.Lens.TH
import           Control.Monad
import qualified Data.Attoparsec.Text as P
import           Data.Either
import           Data.List
import           Data.List.Split
import qualified Data.Map.Strict      as M
import           Data.Maybe
import           Data.Ord
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Debug.Trace
import           Text.Pretty.Simple

type Text = T.Text
type Record = (Text, [Int])

type Memo = M.Map (Int, [Int]) Int

type Path = [Int]
type Result = Either (Int, [Int]) Int

data Tree = Leaf Int Result | Node Int (Int ,[Int]) [Tree]
  deriving (Show, Eq)

main = do
  inputdata <- T.readFile "input"
  let (Right records) = P.parseOnly parseRecords inputdata
  let part1 = sum $ map (length . fitPounds . expand 1) records
  let part2 = sum $ map (getResults . expand 5) records
  print $ part2

getResults :: Record -> Int
getResults (feed, groups) = gr M.empty (Leaf 1 (Left (0, groups))) [1]
  where
    gr :: Memo -> Tree -> Path -> Int
    gr memo (Leaf i (Right ans)) [1] = ans
    gr memo tree path = --traceShow (tree, path) $
      case getFocus tree path of
        (Leaf i (Left (start,gs))) ->
          let newBranch = case fs feed' start gs of
               Right ans -> Leaf i (Right ans) --`debug` "right path"
               Left   [] -> Leaf i (Right 0)   --`debug` "empty result 0"
               Left  sts -> --traceShow "succesful matches" $
                 Node i (start, gs) (zipWith (\st' i' -> Leaf i' (Left (st', tail gs)))
                                     sts [1 .. length sts])
              memo' = case newBranch of
                       Leaf i (Right ans) -> M.insert (start, gs) ans memo
                       _                  -> memo
              tree' = updateTree tree path newBranch
              path' = (getFirstLeftCld (getChildren newBranch):path) --`debug` ("lftchildrenfrom:", newBranch)
          in gr memo' tree' path                       --`debug` ("updated left:",tree', path)
        (Leaf i (Right ans)) -> gr memo tree (tail path) --`debug` ("got a right:", ans) -- , tree, path)
        (Node i stgs chlds) -> case sum <$> mapM getResult chlds of
            Right ans -> let newBranch = Leaf i (Right ans)
                             memo'  = M.insert stgs ans memo
                             tree'  = updateTree tree path newBranch
                         in gr memo' tree' path --(tail path)  `debug` "right merge path"
            Left    _ -> let path' = (getFirstLeftCld chlds:path)
                         in  gr memo tree path' -- `debug` ("go down left:", path', "to:", getFocus tree path')
      where
        feed' = stripDots feed

        fs feed start [] = Right 1
        fs feed start groups = case M.lookup (start, groups) memo of
          Just ans -> Right ans --`debug` ("found memomatch:", ans, (start, groups), fromJust $ M.lookup (start, groups) memo)
          Nothing  -> Left $ findStarts feed groups (head groups) start

getFirstLeftCld :: [Tree] -> Int
getFirstLeftCld [] = error "getFirstLeftCld: no left children"
getFirstLeftCld ((Leaf i (Left v)):ts)  = i
getFirstLeftCld (t:ts)                = getFirstLeftCld ts

findStarts :: Text -> [Int] -> Int -> Int -> [Int]
findStarts txt grs len offset = map ((+1) . (+) offset . toInt)
                                $ findMatches (T.drop offset txt) grs len
  where
    toInt = T.length . T.dropWhileEnd (/= 'm')

findMatches :: Text -> [Int] -> Int -> [Text]
findMatches  txt grs len =
  let result = findMatches' txt len in  filter pruneMatches result
  --`debug` ("txt:", txt, "result:", filter pruneMatches result)
  where
   pruneMatches :: Text -> Bool
   pruneMatches txt = all (\m -> m txt) [m1, m2, m3]
   m2 = T.all (/= '#') . T.take 1 . T.takeWhileEnd (/= 'm')
   m1 = T.all (/= '#') . T.dropWhileEnd (/= 'm')
   m3 t = (length grs /= 1) || T.all (/= '#') t

findMatches' :: Text -> Int -> [Text]
findMatches' txt  len =
  let parsed = filter (not . T.null) . rights
               $ map (\n -> P.parseOnly (parseMatches len n) txt) [0 .. (T.length txt - len)]
      result = parsed
  in result --`debug` ("txt:", txt,"len:", len, "result:", result)

parseMatches:: Int -> Int -> P.Parser Text
parseMatches len st = do
  start <- P.take st
  m <- T.concat <$> P.count len ("#" <|> "?")
  end <- P.takeText
  return $ T.concat [start,  T.replicate len "m", end]

parseRecords :: P.Parser [Record]
parseRecords = P.many1 (parseRecord <* P.skipSpace)

parseRecord :: P.Parser Record
parseRecord = do
  condition <- P.takeTill P.isHorizontalSpace
  P.skipSpace
  groups <- P.many1 (P.decimal <* many (P.char ','))
  return (condition, groups)

findIndexEnd :: (Char -> Bool) -> T.Text -> Maybe Int
findIndexEnd c str = let x = T.findIndex c . T.reverse $ str
                     in  (-) (T.length str - 1) <$> x

expand :: Int -> Record -> Record
expand 1 (s, g) = (s, g)
expand n (s, g) = (T.intercalate "?" . replicate n $ s, concat $ replicate n g)

cmpGroups :: Int -> Int -> Ordering
cmpGroups = comparing Down

countGroups :: Text -> Int
countGroups  = cg . stripDots . T.replace "?" "."
  where
    cg = length . T.splitOn "."

stripDots :: Text -> Text
stripDots = collapseDots . T.dropAround (== '.')
 where
   collapseDots = collapseChar "."

collapseChar :: Text -> Text -> Text
collapseChar x = T.intercalate x . filter (not . T.null) . T.splitOn x

debug :: Show a => b -> a -> b
debug = flip traceShow

startPos :: Text -> Int
startPos = T.length . T.takeWhileEnd (/= 'm')

fitPounds :: Record -> [Text]
fitPounds (str, groups) = filter ((== length groups) . countGroups)
                          . filter (not . T.elem '#') $ foldM fitPounds' str groups

fitPounds' :: Text -> Int -> [Text]
fitPounds' txt  len = let (txtl, txtr) = T.breakOnEnd "m" txt
                          parsed = filter (not . T.null) . rights $
                                   map (\n -> P.parseOnly (parsePounds len n) txtr)
                                   [0 .. (T.length txtr - len)]
                          result = parsed
                           in map (\r -> T.concat [txtl,r]) result

parsePounds:: Int -> Int -> P.Parser Text
parsePounds len st = do
  start <- P.take st
  m <- T.concat <$> P.count len ("#" <|> "?")
  end <- P.takeText
  return $ T.concat [start, T.replicate len "m", end]

getResult :: Tree -> Result
getResult (Node i sg c) = Left (0, snd sg)
getResult (Leaf i v)    = v

getChildren :: Tree -> [Tree]
getChildren (Node i sg c) = c
getChildren (Leaf i v)    = []

getIndex :: Tree -> Int
getIndex (Node i sg c) = i
getIndex (Leaf i v)    = i

setIndex :: Tree -> Int -> Tree
setIndex (Node i sg c) i' = Node i' sg c
setIndex (Leaf i v) i'    = Leaf i' v

getFocus :: Tree -> Path -> Tree
getFocus t p = fcs t (drop 1 . reverse $ p)
  where
    fcs :: Tree -> Path -> Tree
    fcs   t []  = t
    fcs (Node i g cs) (p:ps) =
      let next = head $ filter (\c -> getIndex c == p) cs
      in fcs next ps
    fcs (Leaf i v)    p   = error "focus: Hit leaf prematurely"

updateTree :: Tree -> Path -> Tree -> Tree
updateTree t p v = ut t (drop 1 . reverse $ p) v
  where
    ut :: Tree -> Path -> Tree -> Tree
    ut t [] v = setIndex v (getIndex t)
    ut (Node i sg cs) (p:ps) branch =
      Node i sg $ map (\c -> if getIndex c == p then ut c ps branch else c) cs
    ut _ _ _ = error "ut: path does not exist"


