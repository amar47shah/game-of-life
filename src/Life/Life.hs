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
generation = M.union <$> survived <*> born

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
readLife = fromCells . concatMap liveCells . indexed . boardLines
    where
  liveCells (y, cs) = map (const y <$>) . filter snd . indexed $ map (== live) cs
  indexed = zipWith (,) [0..]
  boardLines = filter isBoardLine . lines
  isBoardLine = and . sequenceA [not . null, (`elem` [live, dead]) . head]
  (live, dead) = ('O', '.')

survived, born :: Life -> Life
survived = M.intersection <*> M.filter (==2) . neighborCounts
born     = M.map (const 1)  . M.filter (==3) . neighborCounts

neighborCounts :: Life -> Life
neighborCounts = M.unionsWith (+) . sequenceA
  [ shift (-1, 1), shift (0, 1), shift (1, 1)
  , shift (-1, 0),               shift (1, 0)
  , shift (-1,-1), shift (0,-1), shift (1,-1)
  ]
