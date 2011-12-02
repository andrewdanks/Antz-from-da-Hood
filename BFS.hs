module BFS where

import qualified Data.Map as Map
import Ants
-- import Bot
import Data.Maybe
import Data.Function
import Data.List

-------------------------------------------------------------------------------
-------Breadth First Search Implementation Module------------------------------
-------------------------------------------------------------------------------

--Return a list of points that form the path from the start node to the end node 
traversePath:: GameParams -> GameState-> Map.Map Point Int -> Point-> Point -> [Point]
--Base case if, you are at the goal, return it
traversePath gp gs map1 start end 
    | start == end = [start] 
    --otherwise, choose adjacent pt with the lowest counter and recurse
    | otherwise = (traversePath gp gs map1 start (head choosepoint)) ++ [end]
    where   adjPoints = getAdjPoint gp gs end
            --the counter to track current point
            counter = fromJust (Map.lookup end map1)      
            --pick one of the adjacent points that has counter - 1 associated with it in the dictionary
            choosepoint = [pt | pt <- adjPoints, ((Map.member pt map1) && ((fromJust (Map.lookup pt map1)) == (counter - 1)))]             
            -- choosepoint = filter (\p -> (fromJust (Map.lookup p map1)) == (counter -1)) adjPoints
                                  

--Get the adjacent points of a particular point
getAdjPoint:: GameParams -> GameState -> Point -> [Point]
getAdjPoint gp gs point = filter (gs `isPointPassable`) [north gp point, south gp point, east gp point, west gp point]

--Get the next point along the calculated path
getNextPoint:: GameParams -> GameState -> Map.Map Point Int -> Point -> Point -> Point
getNextPoint gp gs map2 start end = head (tail (traversePath gp gs map2 start end))

negMod :: Int -> Int -> Int
negMod mp p  | p < 0     = (mp + 1) + p  -- == (mp + 1) - |p|
             | otherwise = p

west :: GameParams -> Point -> Point
west gp pnt =
    let columns = (cols gp)
    in  ((fst pnt), negMod columns (snd pnt) - 1)

east :: GameParams -> Point -> Point
east gp pnt =
    let columns = (cols gp)
    in  ((fst pnt), (snd pnt) + 1 `mod` columns)

south :: GameParams -> Point -> Point
south gp pnt =
    let rowz = (rows gp)
    in  ((fst pnt) + 1 `mod` rowz, (snd pnt))

north :: GameParams -> Point -> Point
north gp pnt = 
    let rowz = (rows gp)
    in  (negMod rowz (fst pnt) - 1, (snd pnt))
    
-- Returns True if the point is "walkable" and False otherwise
isPointPassable :: GameState -> Point -> Bool
isPointPassable gs pt = 
    let pt_tile = tile ((world gs) %! pt)
    in not ((isHillMe pt_tile) || (isWater pt_tile)) 

-- Given a starting point and an ending point, visit and label the level of
-- each point in a BFS search
bfs :: GameParams
    -> GameState
    -> Point -- Start point 
    -> Point -- End point
    -> Map.Map Point Int -- Dictionary of seen points
    -> Int 
    -> [Point] -- Queue
    -> (Map.Map Point Int, Point) -- The dictionary and the end point
bfs gp gs start end dict count queue
    | start == end = (dict, end)
    | count > 20 = (dict, start)
    | queue == [] = (dict, start)
    | otherwise = bfs gp gs newStart end newDict4 newCount (tail newQueue)
    where   n = (north gp start)
            s = (south gp start)
            e = (east gp start)
            w = (west gp start)
            nPoint = if ((isPointPassable gs n) && (not (Map.member n dict))) then n else (-1, -1)
            sPoint = if ((isPointPassable gs s) && (not (Map.member s dict))) then s else (-1, -1)
            ePoint = if ((isPointPassable gs e) && (not (Map.member e dict))) then e else (-1, -1)
            wPoint = if ((isPointPassable gs w) && (not (Map.member w dict))) then w else (-1, -1)
            neighbours = [nPoint, sPoint, ePoint, wPoint]
            queueEnd = map snd $ sortBy (compare `on` fst) [(distance gp pt end, pt) | pt <- neighbours, pt /= (-1, -1)]
            newDict1 = if nPoint /= (-1, -1) then Map.insert nPoint (count + 1) dict else dict
            newDict2 = if sPoint /= (-1, -1) then Map.insert sPoint (count + 1) newDict1 else newDict1
            newDict3 = if ePoint /= (-1, -1) then Map.insert ePoint (count + 1) newDict2 else newDict2
            newDict4 = if wPoint /= (-1, -1) then Map.insert wPoint (count + 1) newDict3 else newDict3
            newQueue = (tail queue) ++ queueEnd
            newStart = head newQueue
            newCount = fromJust (Map.lookup newStart newDict4)
            
