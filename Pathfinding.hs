
module Pathfinding
  ( getNextMove 
  ) where

import Ants
 
{-
nextBestOrder :: Ant -> Point -> Order
nextBestOrder ant dest =
    let
        bestPoint = nextBestPoint (point ant) dest
    in
        Order {ant = ant, }
-}
-- nextBestPoint :: Point -> Point -> Point 
-- nextBestPoint start dest = (point head.pathfind PointCount {point = start, counter = 0} [PointCount {point = dest, counter = 0}])

-- Similar to isDestinationPassable in Bot.hs, but takes Point as parameter instead of Order

{-
inQueueAndLTECount :: PointCount -> [PointCount] -> Bool
inQueueAndLTECount pc queue = length [pc' | pc' <- queue, (point pc') == (point pc), (counter pc') <= (counter pc)] > 0

pathfind :: World -> PointCount -> [PointCount] -> PointCount
pathfind w start queue = 
    let
        -- Adjacent points. TODO: need to mod points with size of map?
        xPlus1       = sumPoint start (0, 1)
        xMinus1      = sumPoint start (0, -1)
        yPlus1       = sumPoint start (1, 0)
        yMinus1      = sumPoint start (-1, 0)
        newCount     = (counter start) + 1
        nearbyPnts   = [PointCount {point = xPlus1, counter = newCount}, PointCount {point = yPlus1, counter = newCount},
                         PointCount {point = xMinus1, counter = newCount}, PointCount {point = yMinus1, counter = newCount}]
        -- Nearby points that can be moved to (not obstacles)
        moveablePnts             = filter (\pc -> isDestinationPassable' w (point pc)) nearbyPnts
        -- If there is an element in the queue with the same coordinate and an equal or lower counter, remove it from the list
        moveablePntsNotInQueue   = filter (\pc -> not.inQueueAndLTECount pc queue) moveablePnts
    in
        pathfind w (head queue) ((tail queue) ++ moveablePntsNotInQueue)
-}


-- Get the next best point in the path by seeing which adjacent point to start has the smallest count.

isDestinationPassable' :: World -> Point -> Bool
isDestinationPassable' w pnt = pos `elem` [Land, Unknown] || isDead pos || isEnemyHill pos
    where
        pos = (tile (w %! pnt))
        isEnemyHill (HillTile owner) = owner /= Me
        isEnemyHill _ = False

-- TODO: What if queue is empty????
nextPoint :: GameParams -> Point -> [(Point, Int)] -> Point
nextPoint gp start [] = start
nextPoint gp start queue | distance gp start next == 1 = next
                         | otherwise = nextPoint gp start (tail queue)
                         where next = fst $ head queue

-- Returns the point the ant should move to
getNextMove :: GameParams -> World -> Point -> Point -> Point
getNextMove gp w start dest = nextPoint gp start (reverse path)
            where path = getPathHelper gp w [(dest,0)] start

getPathHelper :: GameParams -> World -> [(Point, Int)] -> Point -> [(Point, Int)]
getPathHelper gp w queue dest  
    --If you're at dest, stop
    | fst start == dest = [start]
    | counter >= 15 = [start]
    --Otherwise, 
    | otherwise = [start] ++ getPathHelper gp w (tail newQueue) dest
    where start = head queue
          counter = snd start
          -- Nearby points that can be moved to (not obstacles)
          adjPnts = getAdjacentPassablePoints gp w start 
          -- If there is an element in the queue with the same coordinate and an equal or lower  counter, remove it from the list
          newQueue = queue ++ filter (\p -> not $ inQueueAndLTECount p queue) adjPnts

inQueueAndLTECount :: (Point, Int) -> [(Point, Int)] -> Bool
inQueueAndLTECount (pnt, counter) queue = length [pc' | pc' <- queue, pnt == fst pc', snd pc' <= counter] > 0

getAdjacentPassablePoints :: GameParams -> World -> (Point, Int) -> [(Point, Int)]
getAdjacentPassablePoints gp w (pnt, counter) = 
    let
        columns      = (cols gp)
        rowz         = (rows gp)
        newCounter   = counter + 1
        xPlus1       = (((fst pnt) + 1 `mod` columns, (snd pnt)), newCounter)
        xMinus1      = ((negMod columns (fst pnt) - 1, (snd pnt)), newCounter)
        yPlus1       = (((fst pnt), (snd pnt) + 1 `mod` rowz), newCounter)
        yMinus1      = (((fst pnt), negMod rowz (snd pnt) - 1), newCounter)
        -- Adjacent points. TODO: need to mod points with size of map?
        nearbyPnts   = [xPlus1, xMinus1, yPlus1, yMinus1]
    in
        -- Filter points that are not passable
        filter (\p -> isDestinationPassable' w (fst p)) nearbyPnts

-- Ensuring the a row/col value doesn't becomes negative; if so it should go to the max row/col - |p| 
negMod :: Int -> Int -> Int
negMod mp p  | p < 0     = (mp + 1) + p  -- == (mp + 1) - |p|
             | otherwise = p
        
{-
ALGORITHM:
1. First, create a list of coordinates, which we will use as a queue.
2. The queue will be initialized with one coordinate, the end coordinate.
3. Each coordinate will also have a counter variable attached.
4. Then, go through every element in the queue, including elements added to the end over the course of the algorithm, and to each element, do the following:
    - Create a list of the four adjacent cells, with a counter variable of the current element's counter variable + 1
    - Check all cells in each list for the following two conditions:
        - If the cell is a wall, remove it from the list
        - If there is an element in the main list with the same coordinate and an equal or lower counter, remove it from the list
5. Add all remaining cells in the list to the end of the main list
6. Go to the next item in the list
-}

