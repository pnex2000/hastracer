
module Main
    where

import Char
import IO
--import Data.Bits

import Raytracer
import Scene

-- mask lowest 8 bits
mask_ff x = mod x 256
-- shr8, since we're not going to x's bigger than 16 bits
shr8_mask_ff   :: Int -> Int
shr8_mask_ff x = floor ((fromInteger (toInteger x)) / 256.0)

-- hard coded to 256x192 since we have no bitwise operations!
--                                           12
--tgaHeader = map chr [0,0,2,0,0,0,0,0,0,0,0,0,0,1,192,0,24,32]

-- with improvised shifting and masking
tgaHeader x y =
    map chr [0,0,2,0,0,0,0,0,0,0,0,0,(mask_ff x),(shr8_mask_ff x),(mask_ff y),(shr8_mask_ff y),24,32]

-- the image

cgetPx x y = let v = reverse (render_pixel x y)
	     in map (\x -> chr (round (x*255))) v

cgetRow y = foldl1 (++) (map (\x -> cgetPx x y) [0..(swidth-1)])

ctgaImage = foldl1 (++) (map (\y -> cgetRow y) [0..(sheight-1)])

ctgaImage1 = foldl1 (++) (map (\y -> cgetRow y) [0..63])
ctgaImage2 = foldl1 (++) (map (\y -> cgetRow y) [64..127])
ctgaImage3 = foldl1 (++) (map (\y -> cgetRow y) [128..191])

main :: IO ()
main = let s = tgaHeader (floor swidth) (floor sheight)
	   i = ctgaImage
       in do toHandle <- openFile "rttest.tga" WriteMode 
	     hPutStr toHandle s
	     hPutStr toHandle i
             hClose toHandle
             putStr "Done."

