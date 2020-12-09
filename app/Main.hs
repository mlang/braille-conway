module Main where

import Braille.Conway (conway)
import Control.Concurrent (threadDelay)
import System.Console.ANSI (clearScreen, setCursorPosition)

main :: IO ()
main = do
  c <- getContents
  clearScreen
  loop c

loop :: String -> IO ()
loop s = do
  setCursorPosition 0 0
  putStr s
  threadDelay 500000
  loop $! conway s 
