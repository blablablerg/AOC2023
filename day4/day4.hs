{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

import           Control.Applicative
import           Control.Lens
import           Control.Lens.TH
import qualified Data.Attoparsec.Text as P
import           Data.Either
import           Data.List
import qualified Data.Map.Strict      as M
import           Data.Maybe
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Debug.Trace          ()
import           Text.Pretty.Simple


data Card = Card { _id :: Int, _winning_nrs :: [Int], _my_nrs :: [Int]} deriving (Show)
makeLenses ''Card

data Card2 = Card2 {_card :: Card, _multiplier :: Int} deriving (Show)
makeLenses ''Card2

main = do
  inputdata <- T.lines <$> T.readFile "input"
  let cards = rights $ P.parseOnly parseCard <$> inputdata
  let wins = map checkWins cards
  let part1 = sum $ map (\i -> 2^(i - 1)) $ filter (/= 0) wins
  let cards2 = map mkCard2 cards
  let processed_cards2 = processCards2 cards2
  let part_2 = sum $ processed_cards2 ^.. folded . multiplier
  print $ part_2

processCards2 :: [Card2] -> [Card2]
processCards2 = processCards2' []
  where
    processCards2' :: [Card2] -> [Card2] -> [Card2]
    processCards2' result [] = result
    processCards2' result (c:cs) = let wins = checkWins c._card
                                       cs' = cs & taking wins (traverse . multiplier) +~ c._multiplier
                                   in processCards2' (c:result) cs'

mkCard2 :: Card -> Card2
mkCard2 c = Card2 c 1

checkWins :: Card -> Int
checkWins (Card id w_nrs my_nrs) = length $ w_nrs `intersect` my_nrs

parseCard :: P.Parser Card
parseCard = do
  "Card"
  P.skipSpace
  id <- P.decimal
  ":"
  winning_nrs <- P.many1 $ P.skipSpace *> P.decimal
  " |"
  my_nrs <- P.many1 $ P.skipSpace *> P.decimal
  return $ Card id winning_nrs my_nrs

