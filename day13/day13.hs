{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

import           Control.Lens
import           Control.Lens.TH
import           Data.Char
import           Data.List
import           Data.List.Split
import qualified Data.Text  as T
import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as P
import qualified Data.Map.Strict    as M
import           Data.Maybe
import           Data.Either
import           Debug.Trace
import           Text.Pretty.Simple
import           Data.Ord
import           Data.Function

main :: IO ()
main = do
  (Right patterns) <- P.parseOnly parsePatterns <$> T.readFile "input"
  let part1 = sum $ map findScore patterns
  let part2 = sum $ map findScore2 patterns
  print $ part2

findScore2 :: [String] -> Int
findScore2 p = let (oldm, oldl) = head $
                                   findMirror' p  ++ (findMirror' . transpose $ p)
                   hpms = concatMap findMirror' (clean p)
                   vpms = concatMap findMirror' (clean . transpose $ p)
                   hm  = find (\(m,l) -> oldm /= m) hpms
                   vm  = find (\(m,l) -> oldm /= m) vpms
                   hmscore = maybe 0 ((* 100) . length . uncurry (++)) hm
                   vmscore = maybe 0 (length . uncurry (++)) vm
                in hmscore + vmscore

clean :: [String] -> [[String]]
clean p = let joinedp = intercalate "$" p
              rmsmudges = filter (/= joinedp) $
                map (`rmSmudge` joinedp) [0.. length joinedp - 1]
          in  map (splitOn "$") rmsmudges
  where
    rmSmudge i p = let (l, r) = splitAt i p in l ++ (rms (head r) : tail r)
    rms c | c == '$' = '$'
    rms c | c == '#' = '.'
    rms c | c == '.' = '#'

findScore :: [String] -> Int
findScore p = let hMirrorScore =  (* 100) . length . uncurry (++) <$>  findMirror p
                  vMirrorScore =  length . uncurry (++) <$> (findMirror . transpose)  p
                  score  = head $ catMaybes [hMirrorScore, vMirrorScore]
               in score

findMirror :: [String] -> Maybe ([String], [String])
findMirror p = let pMirror = (filter (\(m, notm) -> not (null m))
                             $ map (`foldUp` p) [1 .. length p - 1])
               in listToMaybe pMirror

findMirror' :: [String] -> [([String], [String])]
findMirror' p = let pMirror = (filter (\(m, notm) -> not (null m))
                             $ map (`foldUp` p) [1 .. length p - 1])
                in pMirror

foldUp :: Int -> [String] -> ([String], [String])
foldUp i p = let (l,r) = splitAt i p
                 (l',l'')    = splitAt (length r) $ reverse l
                 (r',r'')    = splitAt (length l) r
                 mirror   = if l' == r' then r' else []
             in  (mirror, reverse l'')

parsePatterns :: P.Parser [[String]]
parsePatterns = do
  map (map T.unpack) <$>
    P.takeWhile1 (not . P.isEndOfLine) `P.sepBy1`  P.endOfLine
    `P.sepBy1` P.skipSpace

printGrid :: [String] -> IO ()
printGrid = putStrLn . unlines

debug :: Show a => b -> a -> b
debug = flip traceShow
