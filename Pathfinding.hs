
data PointCount = PointCount
  { point :: Point
  , counter :: Int
  } deriving (Show)

 
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

isDestinationPassable' :: World -> Point -> Bool
isDestinationPassable' w pnt = pos `elem` [Land, Unknown] || isDead pos || isEnemyHill pos
    where
        pos = (tile (w %! pnt))
        isEnemyHill (HillTile owner) = owner /= Me
        isEnemyHill _ = False

inQueueAndLTECount :: PointCount -> [PointCount] -> Bool
inQueueAndLTECount pc queue = length [pc' | pc' <- queue, (point pc') == (point pc), (counter pc') <= (counter pc)] > 0

pathfind :: World -> PointCount -> [PointCount] -> PointCount
pathfind w start []    = start
pathfind w start queue = 
	let
		-- Adjacent points
		xPlus1		 = sumPoint start (0, 1)
		xMinus1		 = sumPoint start (0, -1)
		yPlus1		 = sumPoint start (1, 0)
		yMinus1		 = sumPoint start (-1, 0)
		newCount	 = (counter start) + 1
		nearbyPnts   = [PointCount {point = xPlus1, counter = newCount}, PointCount {point = yPlus1, counter = newCount},
					     PointCount {point = xMinus1, counter = newCount}, PointCount {point = yMinus1, counter = newCount}]
		-- Nearby points that can be moved to (not obstacles)
		moveablePnts 			 = filter (\pc -> isDestinationPassable' w (point pc)) nearbyPnts
		-- If there is an element in the queue with the same coordinate and an equal or lower counter, remove it from the list
		moveablePntsnotInQueue	 = filter (\pc -> not.inQueueAndLTECount pc queue) moveablePnts
	in
		pathfind w (head queue) ((tail queue) ++ moveablePntsnotInQueue)
		
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

