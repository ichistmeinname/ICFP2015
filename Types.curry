{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module Types where

import Integer (even, odd)
import List (maximum)
import Float (round)
import Maybe (fromJust, isJust)

-- width, height, coordinates (col,row) and fill status
data Board = Board Int Int [Entry]

width :: Board -> Int
width (Board w _ _) = w

height :: Board -> Int
height (Board _ h _) = h

entries :: Board -> [Entry]
entries (Board _ _ e) = e

type Entry = (Coord,Status)
type Coord = (Int,Int)
type Status = Bool
-- data Status = Empty | Full

data Direction = West | SouthEast | East | SouthWest
 deriving Eq

directions = [East,West,SouthEast,SouthWest]

move :: Board -> Direction -> Coord -> Maybe Coord
move (Board w h _) dir (col,row)
  | isValidMove = fmap (\ (f1,f2) -> (f1 col,f2 row)) (lookup dir dirMoves)
 where
  dirMoves = zip directions moves
  moves | even row = [(dec,id),(inc,id),(id,inc),(dec,inc)]
        | odd  row = [(dec,id),(inc,id),(inc,inc),(id,inc)]
  dec = ((-) 1)
  inc = (+ 1)
  isValidMove = case dir of
    West      -> col > 0
    East      -> col < (w-1)
    SouthWest -> row < (h-1) || not (col == 0 && even row)
    SouthEast -> row < (h-1) || not (odd row && col == (w-1))

data Rotation = Clock | CounterClock

data Unit = Unit [Coord] (Coord)

pivot :: Unit -> Coord
pivot (Unit _ c) = c

units :: Unit -> [Coord]
units (Unit cs _) = cs

-- moveUnit :: Board -> Direction -> Unit -> Maybe Unit
moveUnit board dir (Unit cs piv) =
  let newCoords = map (move board dir) cs
  in if all isJust newCoords
       then Just (Unit (map fromJust newCoords) piv)
       else Nothing

-- turn :: Rotation -> Unit -> Unit
-- turn Clock (Unit cs (c,r)) ->

normalise :: Int -> Unit -> Unit
normalise width (Unit cs piv) =
  let colOffset = offset (right + left) (width - 1)
  in Unit (zipWith (,)
                   (map (+ colOffset) cols)
                   (map (\x -> x - top) rows))
          piv
 where
  offset dist orig = (orig `div` 2) - (dist `div` 2) 
  rows = map snd cs
  cols = map fst cs
  top    = minimum (rows)
  left   = minimum (cols)
  right  = maximum (cols)

validUnit :: Board -> Unit -> Bool
validUnit (Board w h es) (Unit cs _) = all (maybe False id . (flip lookup es)) cs

fmap :: (a -> b) -> Maybe a -> Maybe b
fmap f Nothing  = Nothing
fmap f (Just x) = Just (f x)

minimum :: Ord a => [a] -> a
minimum = foldl1 min