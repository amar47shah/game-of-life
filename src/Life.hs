module Life (Life, life, isLive) where

import Life.Life (Life, generation, isLive)
import Life.Pattern

life :: [Life]
life = iterate generation start

start :: Life
start = gosperGliderGun
