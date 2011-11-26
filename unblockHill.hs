-- Remove orders that steps on own hill
isMyHill :: Order -> Bool
isMyHill o = isMyHillHelper o myHills world

isMyHillHelper :: Order -> [Hill] -> World -> Bool
isMyHillHelper o myHills w
	       | myHills == [] = false 
	       | (destination w o) == (pointHill head myHills) = true
	       | otherwise = isMyHillHelper o (tail myHills) w

-- Usage => filter isMyHill [Order]

-- Unblock own hill
unblockMyHill :: [Ant] -> [Hill] -> GameState -> [Order]
unblockMyHill myAnts myHills gs
              | myAnts == [] = []
              | myHills == [] = []
              | otherwise = unblockHillHelper myAnts (head myHills) gs : unblockMyHill (tail myAnts) (tail myHills) 

unblockHillHelper :: [Ant] -> Hill -> GameState -> Order
unblockHillHelper myAnts myHill gs
                  | myAnts == [] = []
                  | pointAnt head myAnts == pointHill myHill =  unblockHillOrder (head myAnts) gs 
                  | otherwise = unblockHillHelper tail myAnts myHill

unblockHillOrder :: Ant -> GameState -> Order
unblockHillOrder ant gs = head (filter (isDestinationPassable (world gs)) (map (Order ant) [North .. West]))
 
 