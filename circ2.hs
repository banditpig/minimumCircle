import Data.Vector ((//), (!))
import qualified Data.Vector as V
import Data.List (delete, find)
import Data.Ord
import Data.List (sortBy)
import Control.Monad.Random (getRandomR)
-- x,y
type Point  = (Float, Float)
-- center (x,y) and radius.
type Circle = (Point, Float)  

makeCircle :: Point -> Point -> Point -> Circle
makeCircle (x1, y1) (x2, y2) (x3, y3) = ((x, y), r2) where
       k = 2 * (x1 * (y2 - y3) - y1 * (x2 - x3) + x2 * y3 - x3 * y2)
       x = ((x1^2 + y1^2) * (y2 - y3) + (x2^2 + y2^2) * (y3 - y1) + (x3^2 + y3^2) * (y1 - y2)) / k
       y = ((x1^2 + y1^2) * (x3 - x2) + (x2^2 + y2^2) * (x1 - x3) + (x3^2 + y3^2) * (x2 - x1)) / k
       r2 = sqrt ((x - x1)^2 + (y - y1)^2)


isInCircle :: Point -> Circle -> Bool
isInCircle (x, y) ((cx, cy), r) = 
    (x - cx)^2 + (y - cy)^2 - r^2 < 0.000001

allCircles :: [Point] -> [Circle]
allCircles ps = [makeCircle p1 p2 p3 | p1 <- ps, p2 <- ps, p3 <- ps, p1 /= p2 && p1 /= p3 && p2 /= p3]  ++ [makeCircle p1 p2 p2| p1 <- ps, p2 <- ps, p1 /= p2 && p2 /= p1]

countPointsIn :: [Point] -> Circle -> Int
countPointsIn [] _ = 0
countPointsIn (p:ps) c 
    | isInCircle p c = 1 + countPointsIn ps c
    | otherwise      =     countPointsIn ps c

isMinCircle :: [Point] -> Circle -> Bool
isMinCircle ps c = countPointsIn ps c == min where min = length ps--div (length ps) 2

getMinCircle :: [Point] -> IO (Maybe Circle)
getMinCircle ps = do 
    ps' <- randomShuffle ps
    let circs = sortBy (comparing snd) . allCircles $ ps'
        maybeList =  dropWhile (\x -> not $ isMinCircle  ps x ) circs
        result = f maybeList where
          f [] = Nothing
          f ls = Just (head ls)
    return result
toPoint :: [String] -> Point
toPoint (x:y:[]) = (read x :: Float, read y :: Float)

showResult :: Maybe Circle -> String
showResult c = 
    case c of
        Nothing           -> "Nothing"
        Just ((x, y), d ) -> show x ++ " " ++ show y ++ " " ++ show d
--  "0.57601506 0.34845096 0.3578915"
-- [Finished in 26.8s]
randomShuffle :: [a] -> IO [a]
randomShuffle [] = return []
randomShuffle lst = do
  i <- getRandomR (0,length lst-1)
  let (a, x:b) = splitAt i lst
  xs <- randomShuffle $ a ++ b
  return (x:xs)

swapShuffle :: Eq a => [a] -> [a] -> [a]
swapShuffle lref lst = V.toList $ foldr adjust (V.fromList lst) [0..n-1]
  where
    vref = V.fromList lref
    n = V.length vref
    adjust i v = case find alternative [0.. n-1] of
      Nothing -> v
      Just j -> v // [(j, v!i), (i, v!j)]
      where
        alternative j = and [ v!i == vref!i
                            , i /= j
                            , v!i /= vref!j
                            , v!j /= vref!i ]
 -- ((0.26316673,0.44963408),0.6069369)
shuffle :: Eq a => [a] -> [a]
shuffle lst = swapShuffle lst lst

makeCircle' :: Point -> Point -> Circle
makeCircle' (x1, y1) (x2, y2) = ( (x, y) , r) where
    x = (x1+x2)/2
    y = (y1+y2)/2
    r = sqrt ((x - x1)^2 + (y - y1)^2)

miniDisc :: [Point] -> Circle
miniDisc (p1:p2:[])    = makeCircle' p1 p2
miniDisc (p1:p2:p3:[]) = makeCircle p1 p2 p3
miniDisc allP@(p1:p2:p3:ps) = go (makeCircle p1 p2 p2)  ([p3]++ ( ps)) ([p1] ++ [p2]) where
  go :: Circle -> [Point] -> [Point] -> Circle
  go c [] _ = c
  go c (p:ls) used 
    | isInCircle p c = go c ls ([p] ++ used)
    | otherwise = go (miniDiscWithPoint used p) ls  ([p] ++ used)

miniDiscWithPoint all@(p1:ps) q = go' (makeCircle' p1 q)  ( all) [p1] where 
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
-- Input. A set P of n points in the plane, and two points q1 and q2 such that there
-- exists an enclosing disc for P with q1 and q2 on its boundary.
-- Output. The smallest enclosing disc for P with q1 and q2 on its boundary.
-- 1. Let D0 be the smallest disc with q1 and q2 on its boundary.
-- 2. for k←1 to n
-- 3. do if pk ∈ Dk−1
-- 4. then Dk ←Dk−1
-- 5. else Dk ←the disc with q1, q2, and pk on its boundary
-- 6. return Dn
checkOnePoint :: [Point] -> Point -> Circle -> Circle
checkOnePoint ps p c 
  | isInCircle p c = c
  | otherwise = miniDiscWithPoint ps p

-- 1. Compute a random permutation p1, . . . , pn of P.
-- 2. Let D2 be the smallest enclosing disc for {p1, p2}.
-- 3. for i←3 to n
-- 4. do if pi ∈ Di−1
-- 5. then Di ←Di−1
-- 6. else Di ← MINIDISCWITHPOINT({p1, . . . , pi−1}, pi)
-- 86 7. return Dn
main :: IO ()
main = do 
    input <- readFile "points.txt"
    let points =  map toPoint .  map words . lines $ input
    -- c <- getMinCircle $ points
    -- print $ showResult c
    -- print "----"
    -- print points
    print $  miniDisc points

-- "0.57601506 0.34845096 0.3578915"
-- [Finished in 25.4s]






