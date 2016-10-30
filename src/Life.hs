module Life (Life, life, isLive) where

import Control.Arrow ((&&&), (***))
import Data.Function.Blackbird ((...))
import qualified Data.Map as M

-- Exported

type Life = M.Map Cell Int

life :: [Life]
life = iterate generation start

isLive :: Cell -> Life -> Bool
isLive = M.member

-- Private

start :: Life
start = backrake

generation :: Life -> Life
generation = uncurry M.union . (survived &&& born)

type Cell = (Int, Int)

survived, born :: Life -> Life
survived b = M.intersection b . M.filter (==2) $ neighborCounts b
born     b = M.map (const 1)  . M.filter (==3) $ neighborCounts b

neighborCounts :: Life -> Life
neighborCounts l = M.unionsWith (+) $ fmap ($ l)
  [ shift (-1, 1), shift (0, 1), shift (1, 1)
  , shift (-1, 0),               shift (1, 0)
  , shift (-1,-1), shift (0,-1), shift (1,-1)
  ]

shift :: Cell -> Life -> Life
shift (dx, dy) = M.mapKeysMonotonic $ (+ dx) *** (+ dy)

fromCells :: [Cell] -> Life
fromCells = M.fromList ... fmap $ id &&& const 1

glider :: Life
glider = shift (12,-8) $ fromCells [(-1,-1),(-1,0),(-1,1),(0,1),(1,0)]

bipole :: Life
bipole = fromCells [(-2,1),(-2,2),(-1,2),(0,1),(0,-1),(1,-2),(2,-2),(2,-1)]

collision :: Life
collision = M.union unknownFormation $ shift (2,-2) glider

unknownFormation :: Life
unknownFormation = shift (-10,-5) $ fromCells
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

backrake :: Life
backrake = shift (-10, 0) $ fromCells
  [ (0,5),(0,7),(1,4),(1,7),(2,3),(2,4),(3,2)
  , (4,1),(4,2),(4,3),(4,4),(5,0),(5,5)
  , (6,0),(6,3),(6,9),(6,10),(6,11)
  , (7,0),(7,3),(7,9),(8,1),(8,9),(8,11)
  , (9,2),(9,3),(9,4),(9,5),(9,7)
  , (10,3),(10,7),(10,10),(11,4),(11,14),(11,15)
  , (12,4),(12,5),(12,6),(12,7),(12,8),(12,12),(12,17)
  , (13,11),(14,4),(14,5),(14,6),(14,7),(14,8),(14,11),(14,17)
  , (15,4),(15,11),(15,12),(15,13),(15,14),(15,15),(15,16)
  , (16,3),(16,7),(17,2),(17,3),(17,4),(17,5),(17,7)
  , (18,1),(18,9),(19,0),(19,3),(19,9)
  , (20,0),(20,3),(20,9),(20,10),(20,12)
  , (21,0),(21,5),(21,12),(22,1),(22,2),(22,3),(22,4)
  , (23,2),(24,3),(24,4),(25,4),(25,7),(26,5),(26,7)
  ]

blinkerShip :: Life
blinkerShip = shift (17,-7) $ fromCells
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

canadaGoose :: Life
canadaGoose = shift (12,12) $ fromCells
  [ (0,0),(0,1),(1,0),(1,2),(2,0),(2,9),(2,10),(2,11)
  , (3,3),(3,7),(3,8),(3,10),(3,11)
  , (4,3),(4,4),(4,6),(5,6),(5,7),(5,8)
  , (7,3),(7,7),(7,9),(8,2),(8,3),(8,5),(8,7),(8,8),(8,9)
  , (9,2),(9,6),(10,1),(10,2),(10,8)
  , (11,1),(11,8),(12,2)
  ]

fromFile :: FilePath -> IO Life
fromFile = fmap readLife . readFile

readLife :: String -> Life
readLife = fromCells . (liveCells =<<) . number . boardLines
    where
  liveCells (y, cs) = map (id *** const y) . filter ((== live) . snd) $ number cs
  number = zipWith (,) [0..]
  boardLines = filter ((`elem` [live, dead]) . head) . filter (not . null) . lines
  (live, dead) = ('O', '.')
