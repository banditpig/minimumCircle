
import Data.Ord
import Data.List (sortBy)
-- x,y
type Point  = (Float, Float)
-- center (x,y) and radius.
type Circle = (Point, Float)  
-- 0.68543744 0.5940232
-- 0.35073838
makeCircle :: Point -> Point -> Point -> Circle
makeCircle (x1, y1) (x2, y2) (x3, y3) = ((x, y), r) where
       k = 2 * (x1 * (y2 - y3) - y1 * (x2 - x3) + x2 * y3 - x3 * y2)
       x = ((x1^2 + y1^2) * (y2 - y3) + (x2^2 + y2^2) * (y3 - y1) + (x3^2 + y3^2) * (y1 - y2)) / k
       y = ((x1^2 + y1^2) * (x3 - x2) + (x2^2 + y2^2) * (x1 - x3) + (x3^2 + y3^2) * (x2 - x1)) / k
       r = sqrt ((x - x1)^2 + (y - y1)^2)

makeCircle' :: Point -> Point -> Circle
makeCircle' (x1, y1) (x2, y2) = ( (x, y) , r) where
    x = (x1+x2)/2
    y = (y1+y2)/2
    r = sqrt ((x - x1)^2 + (y - y1)^2)

isInCircle :: Point -> Circle -> Bool
isInCircle (x, y) ((cx, cy), r) = 
    (x - cx)^2 + (y - cy)^2 - r^2 < 0.000000001

allCircles :: [Point] -> [Circle]
allCircles ps = [makeCircle p1 p2 p3 | p1 <- ps, p2 <- ps, p3 <- ps, p1 /= p2 && p1 /= p3 && p2 /= p3]  ++ [makeCircle p1 p1 p2 | p1 <- ps, p2 <- ps, p1 /= p2 && p2 /= p1]

countPointsIn :: [Point] -> Circle -> Int
countPointsIn [] _ = 0
countPointsIn (p:ps) c 
    | isInCircle p c = 1 + countPointsIn ps c
    | otherwise      =     countPointsIn ps c

isMinCircle :: [Point] -> Circle -> Bool
isMinCircle ps c = countPointsIn ps c == min where min = div (length ps) 2

getMinCircle :: [Point] -> Maybe Circle
getMinCircle ps = result where
    circs = sortBy (comparing snd) . allCircles $ ps
    maybeList =  dropWhile (\x -> not $ isMinCircle  ps x ) circs
    result = f maybeList where
        f [] = Nothing
        f ls = Just (head ls)

toPoint :: [String] -> Point
toPoint (x:y:[]) = (read x :: Float, read y :: Float)

showResult :: Maybe Circle -> String
showResult c = 
    case c of
        Nothing           -> "Nothing"
        Just ((x, y), d ) -> show x ++ " " ++ show y ++ " " ++ show d
--  "0.57601506 0.34845096 0.3578915"
-- [Finished in 26.8s]

main :: IO ()
main = do 
    input <- readFile "points.txt"
    let points =  map toPoint .  map words . lines $ input
    print $  showResult . getMinCircle $ points

-- "0.57601506 0.34845096 0.3578915"
-- [Finished in 25.4s]






