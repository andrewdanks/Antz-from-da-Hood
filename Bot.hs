module Bot where

import Data.List
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import System.IO
import Data.Function

import Debug.Trace

import Ants
import Pathfinding

-------------------------------------------------------------------------------
-- Exploring functionality ----------------------------------------------------
-------------------------------------------------------------------------------

-- Calculate the set of points in the radius of an ant, i.e. the points that an
-- ant can see
-- distanceAnt :: Ant -> GameState -> GameParams -> [Point]
-- distanceAnt ant gs gp = 
--  [p | p <- (indices . world gs), (distance gp p (pointAnt ant)) <= (viewradius2 gp)]
  
-- Given a point and the world, return whether the point is unseen
unseen :: World -> Point -> Bool
unseen w pt = not (seen (w %! pt))

-- Given the game parameters and an ant, return the points within the radius of
-- the ant.
getRadiusPoints :: GameState -> GameParams-> Ant -> [Point]
getRadiusPoints gs gp ant
    | minX >= 0 && maxX <= (cols gp) && maxY <= (rows gp) && minY >= 0 = list1
    | minX < 0 && maxX <= (cols gp) && maxY <= (rows gp) && minY >= 0 = list2
    | minX >= 0 && maxX > (cols gp) && maxY <= (rows gp) && minY >= 0 = list3
    | minX >= 0 && maxX <= (cols gp) && maxY > (rows gp) && minY >= 0 = list4
    | minX >= 0 && maxX <= (cols gp) && maxY <= (rows gp) && minY < 0 = list5
    | minX < 0 && maxX <= (cols gp) && maxY > (rows gp) && minY >= 0 = list6
    | minX < 0 && maxX <= (cols gp) && maxY <= (rows gp) && minY < 0 = list7
    | minX >= 0 && maxX > (cols gp) && maxY > (rows gp) && minY >= 0 = list8
    | minX >= 0 && maxX > (cols gp) && maxY <= (rows gp) && minY < 0 = list9
    where   
        maxX = (fst $ pointAnt ant) + (floor $ sqrt (fromIntegral $ viewradius2 gp)) + 1
        minX = (fst $ pointAnt ant) - (floor $ sqrt (fromIntegral $ viewradius2 gp)) - 1
        maxY = (snd $ pointAnt ant) + (floor $ sqrt (fromIntegral $ viewradius2 gp)) + 1
        minY = (snd $ pointAnt ant) - (floor $ sqrt (fromIntegral $ viewradius2 gp)) - 1
        list1 = [(x, y) | x <- [minX..maxX], y <- [minY..maxY]]
        list2 = [(x, y) | x <- [0..maxX]++[((cols gp) + minX)..(cols gp)], y <- [minY..maxY]]
        list3 = [(x, y) | x <- [minX..(cols gp)]++[0..(maxX `mod` (cols gp))], y <- [minY..maxY]]
        list4 = [(x, y) | x <- [minX..maxX], y <- [minY..(rows gp)]++[0..(maxY `mod` (rows gp))]]
        list5 = [(x, y) | x <- [minX..maxX], y <- [0..maxY]++[((rows gp)+minY)..(rows gp)]]
        list6 = [(x, y) | x <- [0..maxX]++[((cols gp) + minX)..(cols gp)], y <- [minY..(rows gp)]++[0..(maxY `mod` (rows gp))]]
        list7 = [(x, y) | x <- [0..maxX]++[((cols gp) + minX)..(cols gp)], y <- [0..maxY]++[((rows gp)+minY)..(rows gp)]]
        list8 = [(x, y) | x <- [minX..(cols gp)]++[0..(maxX `mod` (cols gp))], y <- [minY..(rows gp)]++[0..(maxY `mod` (rows gp))]]
        list9 = [(x, y) | x <- [minX..(cols gp)]++[0..(maxX `mod` (cols gp))], y <- [0..maxY]++[((rows gp)+minY)..(rows gp)]]

-- Assign each ant that does not have an order already to do some exploring
-- Here, freeants denotes the list of ants that have not been assigned a task
assignExplore :: GameState -> GameParams -> [Ant] -> [(Ant, Point)]
assignExplore gs gp [] = []
assignExplore gs gp (ant:freeants) = 
    -- First finding the range of points that are within view of the ants.
    let unseenPts = [pts | pts <- (getRadiusPoints gs gp ant), unseen (world gs) pts]
        destinations = map snd $ sortBy (compare `on` fst) [(distance gp (pointAnt ant) p, p) | p <- unseenPts]
        order = if destinations == [] then [] else [(ant, head destinations)]
    in  order ++ (assignExplore gs gp freeants)
--assignExplore :: GameState -> GameParams -> [Ant] -> [(Ant, Point)]
--assignExplore gs gp freeants = 
--    let unseenPts = [(x, y) | x <- [0..(rows gp)], y <- [0..(cols gp)], unseen (world gs) (x, y)]
--        orders = map snd $ sortBy (compare `on` fst) [(distance gp (pointAnt a) p,(a,p)) | a <- freeants, p <- unseenPts]
--    in createDictHelper [] [] orders []
        
  
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--directions variable solves repeated calls problem CHANGE
returnOrder :: World -> GameParams -> (Ant, Point) -> Order
returnOrder w gp antpoint 
    | directions == [] = Order{ant = fst antpoint, direction = North} 
    | otherwise = Order{ant = fst antpoint, direction = head (directions)}
    where
        --Compute the next point you should go to
        nextPoint = getNextMove gp w (pointAnt (fst antpoint)) (snd antpoint)
        --Get the direction to that point
    	directions = (getDirections gp (pointAnt (fst antpoint)) nextPoint)
    	
returnOrders :: World -> GameParams -> [(Ant, Point)] ->[Order]
returnOrders w gp [] = []
returnOrders w gp antpoint = map (returnOrder w gp) (antpoint) 

--This change probably wasnt necessary CHANGE
freeAnts :: [(Ant, Point)] -> GameState -> [Ant]
freeAnts antpoint gs = 
    let keys = (Map.keys (Map.fromList antpoint))
    in filter (`notElem` keys) (myAnts (ants gs))

--Helper function 
helpGetClosestPoint :: GameParams -> Ant -> [Point] -> Point -> Point
--takes game parameters  an ant, a list of locations and an initial minimum distance and returns the point closest to the ant
helpGetClosestPoint gp ant points closestpoint 
                | points == [] = closestpoint
                | (distance gp (head points) (pointAnt ant)) < (distance gp (closestpoint) (pointAnt ant)) = helpGetClosestPoint gp ant (tail points) (head points)
                | otherwise = helpGetClosestPoint gp ant (tail points) closestpoint

--Takes a list of points and an ant and returns which point is the closest to it
getClosestPoint :: GameParams -> Ant -> [Point] -> Point
getClosestPoint gp ant (point:points) = helpGetClosestPoint gp ant points point

-- Create a list of (Ant, Point) tuples, where each ant is mapped to a point
createDictHelper :: [Ant] -> [Point] -> [(Ant, Point)] -> [(Ant, Point)] -> [(Ant, Point)]
createDictHelper ants points [] orders_acc = orders_acc
createDictHelper ants points ((ant, point):xs) orders_acc 
                | elem ant ants = createDictHelper ants points xs orders_acc
                | elem point points = createDictHelper ants points xs orders_acc
                | otherwise = createDictHelper (ant : ants) (point : points) xs ((ant, point) : orders_acc)

--Creates a Dictionary of ants and locations, with each ant mapped to its closest food source
createDictionary :: GameParams -> [Ant] -> [Point] -> [(Ant, Point)]
createDictionary gp ants points = 
    let orders = map snd $ sortBy (compare `on` fst) [(distance gp (pointAnt a) p,(a,p)) | a <- ants, p <- points]
    in createDictHelper [] [] orders []

--CreateDictionary gp ants [] = []
--CreateDictionary gp ant:ants (loc:locs) | ant == [] = []
--                                        | otherwise =
--                                            let closestpoint = (getClosestPoint gp ant locs)
--                                            in (closestant, loc):(CreateDictionary gp (removeElem closestant ants) locs)
                              
--Create the dictionary from a list of tuples                          
--createDictionary gp ants locs = Map.fromList (helpCreateDictionary gp ants locs)

-- | Given a origin and destination get the initial movement
getDirections :: GameParams -> Point -> Point -> [Direction]
getDirections gp (x1,y1) (x2,y2) =
    selectConds [
                    (c1&&c11,North), 
                    (c1&&c12,South), 
                    (c2&&c21,South), 
                    (c2&&c22,North), 
                    (c3&&c31,West), 
                    (c3&&c32,East), 
                    (c4&&c41,East), 
                    (c4&&c42,West)] 
    where
        selectConds = map snd .  filter (\t@(a,b) -> a)
        c1 = x1 < x2
        c11 = x2-x1>=halfRows
        c12 = x2-x1<=halfRows
        c2 = x2 < x1
        c21 =x1-x2>=halfRows
        c22 =x2-x2<=halfRows
        c3 = y1<y2
        c31 = y2-y1>=halfCols
        c32 = y2-y1<=halfCols
        c4 = y2<y1
        c41 = y1-y2>=halfCols
        c42 = y1-y2<=halfCols
        halfRows = rows gp `div` 2
        halfCols = cols gp `div` 2

-- For the remaining ants, assignmen them a point to go to
matchRemaining :: [Ant] -> [Point] -> [(Ant, Point)]
matchRemaining ants points = 
    let orders = [(ant, point) | point <- points, ant <- ants]
    in  createDictHelper [] [] orders []

-- | Generates orders for an Ant in all directions
-- TODO: Test code on more difficult map to see if food is top priority
--       Check for unblocking
generateOrders :: GameParams -> GameState -> [Order]
generateOrders gp gs =  (returnOrders gp (foods ++ hilllist++assignList)) ++ extraOrders 
     where
        foods = createDictionary gp (myAnts (ants gs)) (food gs)
        hilllist = createDictionary gp (myAnts (ants gs)) (map pointHill $ filter isEnemy's $ hills gs)
        --Put the hills and food into one dictionary
        --added foodshills to reduce append calls CHANGE
        foodshills = foods ++ hilllist
        --Need a dictionary to filter out these points
        --extraList = matchRemaining (freeAnts foodshills gs) (map snd foodshills)
        assignList = assignExplore gs gp (freeAnts foodshills gs)
        --orders that have not been given to 
        extraOrders =  map (unblockHillOrder gs) (freeAnts (foodshills ++ assignList) gs)

--I think we're always telling the ants to go north here POTENTIAL PROBLEM
unblockHillOrder :: GameState -> Ant -> Order
unblockHillOrder gs ant
    | (filter (isDestinationPassable (world gs)) (orders)) == [] = Order {ant = ant, direction = North}
    | otherwise = head (filter (isDestinationPassable (world gs)) (orders))
    where orders = map (Order ant) [North .. West]

                                            
-- | Avoid Collitions
-- Whe get the set of destinations and generating Order
filterCollidingOrders :: World -> [Order] -> [Order]
filterCollidingOrders w orders =
    let dict = Map.fromList [(destination w order, order) | order <- orders]
    in Map.elems dict
--        theMap = foldl' insertIfNotInMap Map.empty orders
--        insertIfNotInMap :: Map.Map Point Order -> [Order] -> Map.Map Point Order
--        insertIfNotInMap m [] = m
--       insertIfNotInMap m (o:os) = case Map.lookup (destination w o) m of
--            Just _ -> insertIfNotInMap m os
--            Nothing -> Map.insert (destination w o) o m
        
isDestinationPassable :: World -> Order -> Bool
isDestinationPassable w o = pos `elem` [Land, Unknown] || isDead pos || isEnemyHill pos
    where
        newPoint = destination w o
        pos = (tile (w %! newPoint))

        isEnemyHill (HillTile owner) = owner /= Me
        isEnemyHill _ = False

{- |
 - Implement this function to create orders.
 - It uses the IO Monad so algorithms can call timeRemaining.
 -
 - GameParams data holds values that are constant throughout the game
 - GameState holds data that changes between each turn
 - for each see Ants module for more information
 -}
doTurn :: GameParams -> GameState -> IO [Order]
doTurn gp gs = do
  -- generate orders for all ants belonging to me
  let orders = filter (isDestinationPassable (world gs)) (generateOrders gp gs)
  -- this shows how to check the remaining time
  elapsedTime <- timeRemaining gs
  hPutStrLn stderr $ show elapsedTime
  -- wrap list of orders back into a monad
  return (filterCollidingOrders (world gs) orders)
  --return orders
-- | Utils
swap (a,b) = (b,a)

--Removes an element from a list, returns that element
removeElem :: Eq a => a -> [a] -> [a]
removeElem elem1 [] = []
removeElem elem1 (x:xs) | x == elem1 = xs
                        | otherwise = x:(removeElem elem1 xs)
