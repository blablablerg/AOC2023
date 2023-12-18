{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

import           Control.Lens
import           Control.Lens.TH
import qualified Data.Attoparsec.Text as P
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Either
import qualified Data.IntMap.Strict    as M
import           Data.Maybe
import           Debug.Trace
import           Text.Pretty.Simple


data Inst = INS Int String Int | RM Int String
  deriving (Show, Eq)

type Boxes = M.IntMap [(String, Int)]

main :: IO ()
main = do
  inputdata <- splitOn "," . head . lines <$> readFile "input"
  let part1 = sum $ map hash inputdata
  let insts = rights $ P.parseOnly parseInst . T.pack <$> inputdata
  let initBox = M.fromList $ zip [0..255] (replicate 256 [])
  let result  = foldl' exec initBox insts
  let part2  =  sum $ map snd $ focusPower $ M.toAscList result
  print $ part2

focusPower :: [(Int, [(String, Int)])] -> [(String, Int)]
focusPower = concatMap (\(box, lenses) ->
                         zipWith (curry (\((lbl, len), n) ->
                                           (lbl, (box + 1) * n * len)))
                         lenses (reverse [1.. length lenses]))

exec :: Boxes -> Inst -> Boxes
exec bxs (RM i lbl) =
  let box = bxs M.! i
      box' = filter ((/= lbl) . fst) box
  in M.insert i box' bxs
exec bxs (INS i lbl len) =
  let box = bxs M.! i
  in case findIndex ((== lbl) . fst) box of
       Nothing -> M.insert i ((lbl,len):box) bxs
       Just i' -> let box' = box & ix i' .~ (lbl, len)
                  in M.insert i box' bxs

hash :: String -> Int
hash = hash' 0
  where
    hash' v [] = v
    hash' v (c:cs) =
      let v' = ((v + ord c) * 17) `rem` 256
      in hash' v' cs

parseInst :: P.Parser Inst
parseInst = do
  label <- T.unpack <$> P.takeWhile1 isAlpha
  inst  <- P.take 1
  case inst of
    "-" -> return $ RM (hash label) label
    "=" -> INS (hash label) label <$> P.decimal
