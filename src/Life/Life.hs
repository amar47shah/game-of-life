module Life.Life ( Life
                 , fromCells
                 , generation
                 , isLive
                 , shift
                 , together
                 ) where

import Control.Arrow ((&&&), (***))
import Data.Function.Blackbird ((...))
import qualified Data.Map as M

type Life = M.Map Cell Int

fromCells :: [Cell] -> Life
fromCells = M.fromList ... fmap $ id &&& const 1

generation :: Life -> Life
generation = uncurry M.union . (survived &&& born)

isLive :: Cell -> Life -> Bool
isLive = M.member

shift :: Cell -> Life -> Life
shift (dx, dy) = M.mapKeysMonotonic $ (+ dx) *** (+ dy)

together :: Life -> Life -> Life
together = M.union

-- Private

type Cell = (Int, Int)

fromFile :: FilePath -> IO Life
fromFile = fmap readLife . readFile

readLife :: String -> Life
readLife = fromCells . (liveCells =<<) . number . boardLines
    where
  liveCells (y, cs) = map (id *** const y) . filter ((== live) . snd) $ number cs
  number = zipWith (,) [0..]
  boardLines = filter ((`elem` [live, dead]) . head) . filter (not . null) . lines
  (live, dead) = ('O', '.')

survived, born :: Life -> Life
survived b = M.intersection b . M.filter (==2) $ neighborCounts b
born     b = M.map (const 1)  . M.filter (==3) $ neighborCounts b

neighborCounts :: Life -> Life
neighborCounts l = M.unionsWith (+) $ fmap ($ l)
  [ shift (-1, 1), shift (0, 1), shift (1, 1)
  , shift (-1, 0),               shift (1, 0)
  , shift (-1,-1), shift (0,-1), shift (1,-1)
  ]

