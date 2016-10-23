module Life (Board, display, generation, start) where

import Control.Arrow ((&&&), (***))
import qualified Data.Map as M

-- Exported

type Board = M.Map Coordinate Int

generation :: Board -> Board
generation = uncurry M.union . (survived &&& born)

start :: Board
start = M.union glider bipole

display :: Int -> Board -> String
display n b = unlines [ concat [ plot (x, y) b | x <- bounds ] | y <- reverse bounds ]
    where
  bounds = [1 - n..n]
  plot (x, y) = depends "◀▶" "  " . M.member (x, y)
  depends x _ True = x
  depends _ y _    = y

-- Private

type Coordinate = (Int, Int)

survived, born :: Board -> Board
survived b = M.intersection b . M.filter (==2) $ neighborCounts b
born     b = M.map (const 1)  . M.filter (==3) $ neighborCounts b

neighborCounts :: Board -> Board
neighborCounts b = M.unionsWith (+) $ fmap ($ b)
  [ shift (-1, 1), shift (0, 1), shift (1, 1)
  , shift (-1, 0),               shift (1, 0)
  , shift (-1,-1), shift (0,-1), shift (1,-1)
  ]

shift :: Coordinate -> Board -> Board
shift (dx, dy) = M.mapKeysMonotonic $ (+ dx) *** (+ dy)

fromCoordinates :: [Coordinate] -> Board
fromCoordinates = M.fromList ... fmap $ id &&& const 1

glider :: Board
glider = shift (12,-8) $ fromCoordinates [(-1,-1),(-1,0),(-1,1),(0,1),(1,0)]

bipole :: Board
bipole = fromCoordinates [(-2,1),(-2,2),(-1,2),(0,1),(0,-1),(1,-2),(2,-2),(2,-1)]

-- Utilities

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

infixr 8 ...
