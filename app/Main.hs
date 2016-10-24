module Main where

import Life (Board, display, generation, start)

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import System.Process (system)

main :: IO ()
main = void . sequence $ draw . display size <$> frames

frames :: [Board]
frames = take duration $ iterate generation start

draw :: String -> IO ()
draw s = threadDelay wait *> system "clear" *> putStrLn s

size, duration, wait :: Int
size     = 20     -- length of half the side
duration = 160    -- number of frames
wait     = 9*10^4 -- microseconds between frames
