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
import           Debug.Trace
import           Text.Pretty.Simple

data Node = Node {_node :: String, _left :: String, _right :: String} deriving (Eq, Show)
makeLenses ''Node

main = do
  inputdata <- T.readFile "input"
  let (Right (instructions, nodes)) = P.parseOnly parseDocuments inputdata
  let part1 =  traverseNodes instructions nodes (nodes ^?! folded . filtered ((== "AAA") . _node))
                                                (nodes ^?! folded . filtered ((== "ZZZ") . _node))
  let part2 = foldl' getDivisor 1 $
              map (traverseNodes2 instructions nodes) (nodes ^.. folded . filtered ((\n -> last n == 'A') . _node))
  print $ part2

getDivisor :: Int -> Int -> Int
getDivisor = getDivisor' 1

getDivisor' :: Int -> Int -> Int -> Int
getDivisor' i x y = let q =  i * max x y
                        d = min x y
                       in if  mod q d == 0
                          then q
                          else getDivisor' (i + 1) x y

traverseNodes2 :: String -> [Node] -> Node -> Int
traverseNodes2 insts = traverseNodes2' (cycle insts) 0

traverseNodes2' :: String -> Int -> [Node] -> Node -> Int
traverseNodes2' insts n nodes src  = let dest = getNextNode nodes (insts !! n) src
                                           in if last dest._node == 'Z'  then n + 1
                                              else traverseNodes2' insts (n + 1) nodes dest

endZs :: [Node] -> Bool
endZs = allOf (folded . node) (\n -> last n == 'Z')

traverseNodes :: String -> [Node] -> Node -> Node -> Int
traverseNodes insts = traverseNodes' (cycle insts) 0

traverseNodes' :: String -> Int -> [Node] -> Node -> Node -> Int
traverseNodes' insts n nodes src target = let dest = getNextNode nodes (insts !! n) src
                                          in if dest._node == target._node then n + 1
                                             else traverseNodes' insts (n + 1) nodes dest target

getNextNode :: [Node] -> Char -> Node -> Node
getNextNode nodes inst src = let dest = getDest inst src
                             in  nodes ^?! folded . filtered ((== dest) . _node)

getDest :: Char -> Node -> String
getDest 'L' n = n._left
getDest 'R' n = n._right

parseDocuments :: P.Parser (String, [Node])
parseDocuments = do
  instructions <- P.many1 P.letter
  P.skipSpace
  nodes <- P.many1 parseNode
  return (instructions, nodes)

parseNode :: P.Parser Node
parseNode = do
  node <- T.unpack <$> P.take 3
  P.skipSpace >> "=" >> P.skipSpace
  left <- "(" *> (T.unpack <$> P.take 3)
  "," >> P.skipSpace
  right <- T.unpack <$> P.take 3
  ")" >> P.skipSpace
  return $ Node node left right
