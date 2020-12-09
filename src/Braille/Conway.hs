{-# LANGUAGE ViewPatterns #-}
module Braille.Conway ( conway ) where

import Control.Arrow ((***), (&&&))
import Data.Bits (bit, testBit, (.|.), (.&.))
import Data.Char (chr, ord)
import Data.Ix (index, inRange, range)
import Data.Maybe (fromJust)
import Data.Vector.Unboxed (fromList, unsafeIndex)
import Data.Word (Word8)

conway :: String -> String
conway s = unlines $ (map . map) cell grid where
  grid = [[(x*2, y*4) | x <- [0 .. w-1]] | y <- [0 .. h-1]]
  cell loc = chr . foldr (dot loc) 0x2800 . range $ ((0,0), (1,3))
  dot (x, y) (x', y') a
    | alive && neighbours == 2 || neighbours == 3 = a .|. bit (toBit (x', y'))
    | otherwise                                   = a
   where
    origin = (x+x', y+y')
    alive = isSet origin
    neighbours = length . take 4 . filter id . map isSet . neighbourCoords $
                 origin
  isSet (bindex (w, h) -> (vi, b)) = vec `unsafeIndex` vi `testBit` b
  lns = lines s
  (w, h) = (maximum $ length <$> lns, length lns)
  vec = fromList . concatMap embossAndPad $ lns
  embossAndPad x = map emboss x <> replicate (w - length x) 0
  emboss (ord -> c)
    | inRange (0x2800, 0x28FF) c = fromIntegral $ c .&. 0XFF
    | otherwise                  = 0 :: Word8

bindex (w, h) (x, y) = (i, b) where
  i = index ((0, 0), (h - 1, w - 1)) (y', x')
  b = toBit (x'', y'')
  (y', y'') = (y `mod` (h * 4)) `divMod` 4
  (x', x'') = (x `mod` (w * 2)) `divMod` 2

toBit = fromJust . (`lookup` zip (range ((0,0), (1,2)) <> [(0,3), (1,3)]) [0..])

neighbourCoords c =
  filter (/= c) . range $ ((pred *** pred) &&& (succ *** succ)) c
