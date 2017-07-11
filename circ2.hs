import Data.Ord

type Point  = (Float, Float)
type Circle = (Point, Float)  

makeCircle' :: Point -> Point -> Circle
makeCircle' (x1, y1) (x2, y2) = ( (x, y) , r) where
    x = (x1+x2)/2
    y = (y1+y2)/2
    r = sqrt ((x - x1)^2 + (y - y1)^2)

makeCircle :: Point -> Point -> Point -> Circle
makeCircle (x1, y1) (x2, y2) (x3, y3) = ((x, y), r2) where
       k = 2 * (x1 * (y2 - y3) - y1 * (x2 - x3) + x2 * y3 - x3 * y2)
       x = ((x1^2 + y1^2) * (y2 - y3) + (x2^2 + y2^2) * (y3 - y1) + (x3^2 + y3^2) * (y1 - y2)) / k
       y = ((x1^2 + y1^2) * (x3 - x2) + (x2^2 + y2^2) * (x1 - x3) + (x3^2 + y3^2) * (x2 - x1)) / k
       r2 = sqrt ((x - x1)^2 + (y - y1)^2)

isInCircle :: Point -> Circle -> Bool
isInCircle (x, y) ((cx, cy), r) = 
    (x - cx)^2 + (y - cy)^2 - r^2 < 0.000001

toPoint :: [String] -> Point
toPoint (x:y:[]) = (read x :: Float, read y :: Float)

miniDisc :: [Point] -> Circle
miniDisc (p1:p2:[])         = makeCircle' p1 p2
miniDisc (p1:p2:p3:[])      = makeCircle p1 p2 p3
miniDisc allP@(p1:p2:p3:ps) = go (makeCircle' p1 p2)  ([p3] ++ ps) ([p1] ++ [p2]) where
  go :: Circle -> [Point] -> [Point] -> Circle
  go c [] _ = c
  go c (p:ls) used 
    | isInCircle p c = go c ls ([p] ++ used)
    | otherwise = go (miniDiscWithPoint used p) ls  ([p] ++ used)

miniDiscWithPoint all@(p1:ps) q = go' (makeCircle' p1 q) all [p1] where 
  go' :: Circle -> [Point] -> [Point] -> Circle
  go' c [] _ = c
  go' c (p:ls) used 
    | isInCircle p c = go' c ls ([p] ++ used)
    | otherwise = go' (miniDiscWithTwoPoints used p q) ls  ([p] ++ used)

miniDiscWithTwoPoints pts q1 q2 = go'' (makeCircle' q1 q2) ( pts) q1 q2 where 
   go'' c [] _ _ = c
   go'' c (p:ps) q1 q2
      | isInCircle p c = go'' c ps q1 q2
      | otherwise = makeCircle p q1 q2

main :: IO ()
main = do 
    input <- readFile "points.txt"
    let points =  map toPoint .  map words . lines $ input
    print $  miniDisc points







