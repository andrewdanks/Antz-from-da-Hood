module Util
  ( modDistance 
  , manhattan
  , tuplify2
  , twoNormSquared
  , distance
  , sumPoint
  , incPoint
  , modPoint
  , getPointCircle
  , select
  , fAnd
  , fOr
  ) where

modDistance :: Int -- modulus
            -> Int -> Int -> Int
modDistance m x y = 
  let a = abs $ x - y
  in min a (m - a)

-- | Computes manhattan distance.
manhattan :: Point -- modulus point
          -> Point -> Point -> Int
manhattan mp p1 p2 =
  let rowd = modDistance (row mp) (row p1) (row p2)
      cold = modDistance (col mp) (col p1) (col p2)
  in rowd + cold

-- | Computes the square of the two norm.
twoNormSquared :: Point -> Int
twoNormSquared p = row p ^ (2::Int) + col p ^ (2::Int)

distance :: GameParams -> Point -> Point -> Int
distance gp p1 p2 =
  let mp = (rows gp, cols gp)
  in manhattan mp p1 p2

sumPoint :: Point -> Point -> Point
sumPoint x y = (row x + row y, col x + col y)

incPoint :: Point -> Point
incPoint = sumPoint (1,1)

modPoint :: Point -- modulus point
         -> Point -> Point
modPoint mp p = (row p `mod` row mp, col p `mod` col mp)

getPointCircle :: Int -- radius squared
               -> [Point]
getPointCircle r2 =
  let rx = truncate.sqrt.(fromIntegral::Int -> Double) $ r2
  in filter ((<=r2).twoNormSquared) $ (,) <$> [-rx..rx] <*> [-rx..rx]

fAnd :: a -> [a -> Bool] -> Bool
fAnd x = all ($x)

fOr :: a -> [a -> Bool] -> Bool
fOr x = any ($x)

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)


-- select is less ambiguous than filter
select :: (a -> Bool) -> [a] -> [a]
select cond inThis = filter cond inThis



