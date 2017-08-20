module Main where

import           Codec.Picture
import           Linear        (V2 (..))
import           Linear.Metric (distance)
import           Perlin        (Perlin)
import qualified Perlin

main :: IO ()
main =
  --writePng "example.png" $ generateImage (filledCircle 512) 512 512
  writePng "perlin1.png" $ generateImage (perlinImage Perlin.init 32) 512 512

perlinImage :: Perlin -> Double -> Int -> Int -> PixelRGB8
perlinImage perlin freq x y =
    let x' = fromIntegral x / 512
        y' = fromIntegral y / 512
        nse = clamp (-1) 1 $ Perlin.noise perlin (x' * freq) (y' * freq)
        px = toPixel nse
    in PixelRGB8 px px px

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

toPixel :: Double -> Pixel8
toPixel val = floor $ (val + 1) * 100

clamp :: Double -> Double -> Double -> Double
clamp mi ma = max mi . min ma
