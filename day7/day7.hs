{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

import           Control.Applicative
import           Control.Lens
import           Control.Lens.TH
import qualified Data.Attoparsec.Text as P
import           Data.Char
import           Data.Either
import           Data.List
import qualified Data.Map.Strict      as M
import           Data.Maybe
import           Data.Ord
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Debug.Trace          ()
import           Text.Pretty.Simple

data CardBid = CardBid {_hand :: String, _bid :: Int} deriving (Eq, Show)
makeLenses ''CardBid

data CardBid2 = CardBid2 {_hand2 :: (String, String), _bid2 :: Int} deriving (Eq, Show)
makeLenses ''CardBid2

instance Ord CardBid where
  compare = comparing (handType . _hand) <> comparing (hand2Ints . _hand)

instance Ord CardBid2 where
  compare = comparing (handType . fst . _hand2) <>
            comparing (hand2Ints . snd . _hand2)

main = do
  inputdata <- T.readFile "input"
  let (Right cardbids) = P.parseOnly parseCardBids inputdata
  let part1 = snd . foldl' (\acc cardbid -> (fst acc + 1, snd acc + fst acc * cardbid._bid)) (1,0) $ sort cardbids
  let cardbids2 = map mkCard2 cardbids
  let part2 = snd . foldl' (\acc cardbid2 -> (fst acc + 1, snd acc + fst acc * cardbid2._bid2)) (1,0) $ sort cardbids2
  print $ part2

mkCard2 :: CardBid -> CardBid2
mkCard2 (CardBid hand bid) = CardBid2 (convertJokers hand, convertJokerTo1 hand) bid

convertJokers :: String -> String
convertJokers hand = minimumBy (comparing (Down . handType) <> comparing (Down . hand2Ints)) (explodeJokers hand)

explodeJokers :: String -> [String]
explodeJokers hand = explodeJokers' 0 [hand]
  where
    explodeJokers' :: Int -> [String] -> [String]
    explodeJokers' 5 hands = hands
    explodeJokers' n hands = explodeJokers' (n+1) (concatMap (explodeJoker n) hands)

explodeJoker :: Int -> String -> [String]
explodeJoker i hand | hand !! i == 'J' = map (\n -> hand & ix i .~ n) (map intToDigit [1 .. 9] ++ ['A', 'K', 'Q', 'T'])
                    | otherwise       = [hand]

convertJokerTo1 :: String -> String
convertJokerTo1 hand = hand & each %~ (\c -> if c == 'J' then '1' else c)

handType :: String -> [Int]
handType = sortOn Down . map length  . group . sort

hand2Ints :: String -> [Int]
hand2Ints = map card2Int

card2Int :: Char -> Int
card2Int c | isDigit c = digitToInt c
           | otherwise = pictureMap M.! c

pictureMap :: M.Map Char Int
pictureMap = M.fromList $ zip ['A','K', 'Q', 'J', 'T'] [14,13,12,11,10]

parseCardBids :: P.Parser [CardBid]
parseCardBids = P.many1 parseCardBid

parseCardBid :: P.Parser CardBid
parseCardBid = do
  hand <- T.unpack <$> P.take 5
  P.skipSpace
  bid <- P.decimal
  P.skipSpace
  return $ CardBid hand bid

