module Main where

import           Codec.Picture
import           Linear        (V2 (..))
import           Linear.Metric (distance)

main :: IO ()
main =
  writePng "example.png" $ generateImage (filledCircle 512) 512 512

filledCircle :: Int -> Int -> Int -> PixelRGB8
filledCircle dimension x y =
    let mid = fromIntegral dimension / 2 :: Float
        origo = V2 mid mid
        dist = distance origo $ V2 (fromIntegral x) (fromIntegral y)
    in
        if dist < mid then
            PixelRGB8 255 0 0
        else
            PixelRGB8 0 0 0
