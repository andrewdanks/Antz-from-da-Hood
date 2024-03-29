module Bot where

import Data.List
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import System.IO
import Data.Function

import Debug.Trace

import Ants

returnOrder :: GameParams -> (Ant, Point) -> Order
returnOrder gp antpoint 
    | (getDirections gp (pointAnt (fst antpoint)) (snd antpoint)) == [] = Order{ant = fst antpoint, direction = North} 
    | otherwise = Order{ant = fst antpoint, direction = head (getDirections gp (pointAnt (fst antpoint)) (snd antpoint))}

returnOrders :: GameParams -> [(Ant, Point)] ->[Order]
returnOrders gp [] = []
returnOrders gp antpoint = map (returnOrder gp) (antpoint) 

freeAnts :: [(Ant, Point)] -> GameState -> [Ant]
freeAnts antpoint gs = filter (`notElem` (Map.keys (Map.fromList antpoint))) (myAnts (ants gs))

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

createDictHelper :: [(Ant, Point)] -> [(Ant, Point)] -> [(Ant, Point)]
createDictHelper [] orders_acc = orders_acc
createDictHelper ((ant, point):xs) orders_acc 
                | elem ant [ants | (ants, points) <- orders_acc] = createDictHelper xs orders_acc
                | elem point [points | (ants, points) <- orders_acc] = createDictHelper xs orders_acc
                | otherwise = createDictHelper xs ((ant, point) : orders_acc)

--Creates a Dictionary of ants and locations, with each ant mapped to its closest food source
createDictionary :: GameParams -> [Ant] -> [Point] -> [(Ant, Point)]
createDictionary gp ants points = 
    let orders = map snd $ sortBy (compare `on` fst) [(distance gp (pointAnt a) p,(a,p)) | a <- ants, p <- points]
    in createDictHelper orders []

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

-- | Generates orders for an Ant in all directions
-- TODO: Test code on more difficult map to see if food is top priority
--       Check for unblocking
generateOrders :: GameParams -> GameState  -> [Order]
generateOrders gp gs =  (returnOrders gp (foods ++ hilllist)) ++ extraOrders
     where
        foods = createDictionary gp (myAnts (ants gs)) (food gs)
        hilllist = createDictionary gp (myAnts (ants gs)) (map pointHill $ filter isEnemy's $ hills gs)
        extraOrders =  map (unblockHillOrder gs) (freeAnts (foods ++ hilllist) gs)

unblockHillOrder :: GameState -> Ant -> Order
unblockHillOrder gs ant
    | (filter (isDestinationPassable (world gs)) (map (Order ant) [North .. West])) == [] = Order {ant = ant, direction = North}
    | otherwise = head (filter (isDestinationPassable (world gs)) (map (Order ant) [North .. West]))
                                            
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