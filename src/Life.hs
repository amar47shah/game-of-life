module Life (Board, display, generation, start) where

import Control.Arrow ((&&&), (***))
import Data.Bool (bool)
import qualified Data.Map as M

-- Exported

type Board = M.Map Coordinate Int

generation :: Board -> Board
generation = uncurry M.union . (survived &&& born)

start :: Board
start = canadaGoose

display :: Int -> Board -> String
display n b =
  unlines [ concat [ plot (x, y) b | x <- bounds ] | y <- reverse bounds ]
    where
  bounds = [1 - n..n]
  plot = bool "  " "◀▶" ... M.member

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

collision :: Board
collision = M.union unknownFormation $ shift (2,-2) glider

unknownFormation :: Board
unknownFormation = shift (-10,-5) $ fromCoordinates
  [ (13,12),(12,12),(11,12),(10,12)
  , (14,11),(10,11),(10,10),(14,9),(11,9),(2,9),(1,9)
  , (8,8),(4,8),(3,8),(2,8),(1,8)
  , (26,7),(25,7),(24,7),(19,7),(10,7),(9,7),(7,7),(3,7),(2,7)
  , (26,6),(24,6),(19,6),(10,6),(6,6)
  , (26,5),(25,5),(24,5),(19,5),(10,5),(9,5),(7,5),(3,5),(2,5)
  , (8,4),(4,4),(3,4),(2,4),(1,4)
  , (14,3),(11,3),(2,3),(1,3),(10,2),(14,1),(10,1)
  , (13,0),(12,0),(11,0),(10,0)
  ]

blinkerShip :: Board
blinkerShip = shift (17,-7) $ fromCoordinates
  [ (10,0),(11,0),(12,0),(13,0)
  , (10,1),(14,1),(10,2),(1,3),(2,3),(11,3),(14,3)
  , (0,4),(1,4),(3,4),(4,4)
  , (1,5),(2,5),(3,5),(4,5),(8,5)
  , (2,6),(3,6),(7,6),(9,6),(10,6),(19,6),(24,6),(25,6),(26,6)
  , (6,7),(10,7),(19,7),(24,7),(26,7)
  , (2,8),(3,8),(7,8),(9,8),(10,8),(19,8),(24,8),(25,8),(26,8)
  , (1,9),(2,9),(3,9),(4,9),(8,9)
  , (0,10),(1,10),(3,10),(4,10)
  , (1,11),(2,11),(11,11),(14,11),(10,12),(10,13),(14,13)
  , (10,14),(11,14),(12,14),(13,14)
  ]

canadaGoose :: Board
canadaGoose = shift (12,12) $ fromCoordinates
  [ (0,0),(0,1),(1,0),(1,2),(2,0),(2,9),(2,10),(2,11)
  , (3,3),(3,7),(3,8),(3,10),(3,11)
  , (4,3),(4,4),(4,6),(5,6),(5,7),(5,8)
  , (7,3),(7,7),(7,9),(8,2),(8,3),(8,5),(8,7),(8,8),(8,9)
  , (9,2),(9,6),(10,1),(10,2),(10,8)
  , (11,1),(11,8),(12,2)
  ]

fromFile :: FilePath -> IO Board
fromFile = fmap readBoard . readFile

readBoard :: String -> Board
readBoard = fromCoordinates . (liveCoords =<<) . number . boardLines
    where
  liveCoords (y, cs) = map (id *** const y) . filter ((== live) . snd) $ number cs
  number = zipWith (,) [0..]
  boardLines = filter ((`elem` [live, dead]) . head) . filter (not . null) . lines
  (live, dead) = ('O', '.')

-- Utilities

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

infixr 8 ...
