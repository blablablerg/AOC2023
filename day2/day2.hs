{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

import           Control.Applicative
import           Control.Lens
import           Control.Lens.TH
import qualified Data.Attoparsec.Text as P
import           Data.Either          (rights)
import qualified Data.Map.Strict      as M
import           Data.Maybe
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Debug.Trace          ()
import           Text.Pretty.Simple

newtype Game = Game Int deriving (Show, Eq, Ord)

data Gameset = Gameset { _red :: Int, _blue :: Int, _green :: Int} deriving (Show)
makeLenses ''Gameset

gamesetDefault = Gameset {_red = 0, _blue = 0, _green = 0}
gamePlayed = Gameset {_red = 12, _blue = 14, _green = 13}


main = do
  inputdata <- T.lines <$> T.readFile "input"
  let games = M.fromList . rights $ P.parseOnly parseGame <$> inputdata
  let filteredGames = M.filter (all (possibleGame gamePlayed)) games
  let part1 = sum $ [i | (Game i) <- M.keys filteredGames]
  let maxgames = M.map (powerSet . maxGamesets) games
  let part2 = sum $ M.elems maxgames
  pPrint part2

powerSet :: Gameset -> Int
powerSet g = g._red * g._green * g._blue

possibleGame :: Gameset -> Gameset -> Bool
possibleGame g1 g2 = let r = g1._red >= g2._red
                         b = g1._blue >= g2._blue
                         g = g1._green >= g2._green
                      in r && b && g

mergeGamesets :: [Gameset] -> Gameset
mergeGamesets = foldr sumGamesets gamesetDefault

sumGamesets :: Gameset -> Gameset -> Gameset
sumGamesets g1 g2 = Gameset {_red = g1._red + g2._red,
                             _blue = g1._blue + g2._blue,
                             _green = g1._green + g2._green}

maxGamesets :: [Gameset] -> Gameset
maxGamesets gs = foldr maxGame (head gs) (tail gs)
  where
    maxGame :: Gameset -> Gameset -> Gameset
    maxGame g1 g2 = Gameset {_red = max g1._red g2._red,
                             _green = max g1._green g2._green,
                             _blue = max g1._blue g2._blue}

parseGame :: P.Parser (Game, [Gameset])
parseGame = do
  P.take 5
  game <- Game <$> P.decimal
  P.take 2
  gamesets <- (mergeGamesets <$> P.many1 parseCube) `P.sepBy` P.string "; "
  return  (game, gamesets)

parseCube :: P.Parser Gameset
parseCube = do
  n <- P.decimal
  P.skipSpace
  color <- P.string "blue" <|> P.string "red" <|> P.string "green"
  many $ P.string ", "
  return (toGameset n color)

toGameset :: Int -> T.Text -> Gameset
toGameset n "blue"  = set blue n gamesetDefault
toGameset n "red"   = set red n gamesetDefault
toGameset n "green" = set green n gamesetDefault
