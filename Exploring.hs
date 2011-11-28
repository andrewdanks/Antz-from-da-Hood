module Exploring
  ( removeUnseen 
  , bestDirection
  , antAtLocation
  , getExploreOrder
  ) where

{-
This should replace the GameState data type in Ants.hs
I wonder if we should also put orders :: [Order] in it.

data GameState = GameState
  { world :: World
  , ants :: [Ant]
  , food :: [Food] -- call "food GameState" to return food list
  , hills :: [Point]
  , unseen :: [Point]
  , startTime :: UTCTime
  }


initialGameState :: GameParams -> UTCTime -> GameState
initialGameState gp time =
  let world'  = listArray ((0,0), (rows gp - 1, cols gp - 1)) (repeat MetaTile {tile = Unknown, visible = False})
      allRows = [0..rows gp - 1]
      allCols = [0..cols gp - 1]
      unseen' = [(Point x y) | x <- allRows, y <- allCols]
  in GameState
    { world = world'
    , ants = []
    , food = []
    , unseen = unseen'
    , hills = []
    , startTime = time
    }
   
   The function above is not defined in the new starter code.
   Need to figure out how to give the unseen paramter in GameState
   an initial value in the new starter code.

  -}

{-
Every time the GameState is updated, this needs to be called
to remove the points that were unseen by an ant.
-}
--removeUnseen :: GameState -> Point -> GameState
--removeUnseen gs p =
--  let unseen' = select (not . p) (unseen gs)
--  in GameState
--    { world = newWorld
--    , ants = ants gs
--    , food = food gs
--    , unseen = unseen'
--    , hills = hills gs
--    , startTime = startTime gs
--    }
    
-- Calculate the set of points in the radius of an ant, i.e. the points that an
-- ant can see
distanceAnt :: Ant -> GameState -> GameParams -> [Point]
distanceAnt ant gs gp = 
  [p | p <- (indices . world gs), (distance gp p (pointAnt ant)) <= (viewradius2 gp)]

{-
This may or may not be useful for other people.
-}
--antAtLocation :: Point -> Ant
--antAtLocation p = head . filter (\a -> point a == p) myAnts

-- Given a point and the world, return whether or not the point has been seen
seen :: World -> Point -> Bool
seen w pt = not (unseen (w %! pt))

-- Return a list of points that are not yet seen
filterUnseen :: GameState -> GameParams -> [Point]
filterUnseen gs gp =
  let allPoints = [(x, y) | x <- [0..(rows gp)], x <- [0..(cols gp)]] 
  in [point | point <- allPoints, seen (world gs) point] 
  
 
-- Assign each ant that does not have an order already to do some exploring
-- Here, freeants denotes the list of ants that have not been assigned a task

assignExplore :: GameState -> GameParams -> [Ant] -> [Order]
assignExplore gs gp freeants = 
  let points_ants = assignExploreHelper gs gp freeants
      ant_dirs = [(ant, head (getDirections gp (pointAnt ant) point)) | (point, ant) <- points_ants]
  in [Order {ant = ant1, direction = dir1} | (ant1, dir1) <- ant_dirs]

-- TODO: orders is not in GameState yet! We probably need to keep track of orderes
-- so we don't assign the same Ant more than one order. My function filters Ants
-- who already have orderes assigned to them.
-- getExploreOrder :: GameParams -> GameState -> Order
-- getExploreOrder gp gs = 
--  let ants = filter (\a -> a `notElem` orders gs) myAnts
--      unseen_dist = sort [(distance gp loc1 (point ant), loc1) | loc1 <- gs unseen, ant <- ants]
--  in snd . head . dropWhile (\(w, o) -> passable w o)
--      [(world gs, (Order ant (bestDirection (point ant) unseen_loc))) | (dist, unseen_loc) <- unseen_dist, ant <- ants]
