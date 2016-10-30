module Main where

import Life (Life, life, isLive)

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Bool (bool)
import Data.Function.Blackbird ((...))
import System.Process (system)

main :: IO ()
main = void . sequence . map (draw wait . display size) . take ticks $ life

display :: Int -> Life -> String
display n b =
  unlines [ concat [ plot (x, y) b | x <- bounds ] | y <- reverse bounds ]
    where
  bounds = [1 - n..n]
  plot = bool "  " "◀▶" ... isLive

draw :: Int -> String -> IO ()
draw n s = threadDelay n *> system "clear" *> putStrLn s

size, ticks, wait :: Int
size  = 20     -- length of half the side
ticks = 160    -- number of frames
wait  = 9*10^4 -- microseconds between frames
