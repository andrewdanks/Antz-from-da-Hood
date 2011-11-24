module Exploring
  ( removeUnseen 
  , bestDirection
  , antAtLocation
  , getExploreOrder
  ) where

-- Just import this file into Ants.hs

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
removeUnseen :: GameState -> Point -> GameState
removeUnseen gs p =
  let unseen' = select (not . p) (unseen gs)
  in GameState
    { world = newWorld
    , ants = ants gs
    , food = food gs
    , unseen = unseen'
    , hills = hills gs
    , startTime = startTime gs
    }


{-
The function is probably useless since the new starter
code already has something that's probably like it
-}
bestDirection :: Point -> Point -> Direction
bestDirection from to
  | col from > col to  = East
  | col from < col to   = West
  | row from > row to  = South
  | otherwise   = North

{-
This may or may not be useful for other people.
-}
antAtLocation :: Point -> Ant
antAtLocation p = head . filter (\a -> point a == p) myAnts

-- TODO: orders is not in GameState yet! We probably need to keep track of orderes
-- so we don't assign the same Ant more than one order. My function filters Ants
-- who already have orderes assigned to them.
getExploreOrder :: GameParams -> GameState -> Order
doExplore gp gs = 
  let ants = filter (\a -> a `notElem` orders gs) myAnts
      unseen_dist = sort [(distance gp loc1 (point ant), loc1) | loc1 <- gs unseen, ant <- ants]
  in snd . head . dropWhile (\(w, o) -> passable w o)
      [(world gs, (Order ant (bestDirection (point ant) unseen_loc))) | (dist, unseen_loc) <- unseen_dist, ant <- ants]

 