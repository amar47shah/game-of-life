module Life where

import Control.Concurrent (threadDelay)
import Control.Arrow ((&&&), (***))
import Control.Monad (void)
import System.Process (system)
import qualified Data.Map as M

type Coordinate = (Int, Int)
type Board = M.Map Coordinate Int

main :: IO ()
main = void . sequence $ draw . display 10 <$> frames

frames :: [Board]
frames = take 100 $ iterate generation $ M.union (shift (10, -10) glider) bipole

glider :: Board
glider = fromCoordinates [(1,1),(1,2),(1,3),(2,3),(3,2)]

bipole :: Board
bipole = fromCoordinates [(-2,1),(-2,2),(-1,2),(0,1),(0,-1),(1,-2),(2,-2),(2,-1)]

draw :: String -> IO ()
draw s = threadDelay (10^5) *> system "clear" *> putStrLn s

-- 2n x 2n display
display :: Int -> Board -> String
display n = unlines . M.foldrWithKey plot blank
    where
  blank = replicate (2*n) $ replicate (2*n) ' '
  inBounds k = -n < k && k <= n
  plot (x, y) _ p
   | inBounds x && inBounds y = let (j, k:l) = splitAt (n - y) p
                                 in let (a, _:c) = splitAt (n - 1 + x) k
                                     in j ++ [a ++ "*" ++ c] ++ l
   | otherwise                = p

fromCoordinates :: [Coordinate] -> Board
fromCoordinates = M.fromList ... fmap $ id &&& const 1

shift :: Coordinate -> Board -> Board
shift (dx, dy) = M.mapKeysMonotonic $ (+ dx) *** (+ dy)

neighborCounts :: Board -> Board
neighborCounts b = M.unionsWith (+) $ fmap ($ b)
  [ shift (-1,-1), shift (0,-1), shift (1,-1)
  , shift (-1, 0),               shift (1, 0)
  , shift (-1, 1), shift (0, 1), shift (1, 1) ]

generation :: Board -> Board
generation = uncurry M.union . (survived &&& born)

survived, born :: Board -> Board
survived b = M.intersection b . M.filter (==2) $ neighborCounts b
born     b = M.map (const 1)  . M.filter (==3) $ neighborCounts b

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

infixr 8 ...
