{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

data Point = Point { x :: Int, y :: Int}

newPoint :: Point
newPoint = Point 3 4

square :: Int -> Int
square x = x * x

distance :: Int -> Int -> Int
distance x y = abs $ x - y

sqDistance :: Int -> Int -> Int
sqDistance x y = square $ distance x y

getDistance :: Point -> Point -> Float
getDistance p1 p2 = sqrt . fromIntegral $ sqDistance p1.x p2.x - sqDistance p1.y p2.y -- :: Float
