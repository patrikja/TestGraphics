module Lib
    ( test
    ) where
import Graphics.Gloss
import System.Environment

import Data.Complex
import Graphics.Gloss.Data.Color (Color)
import Graphics.Gloss.Data.Display (Display(InWindow))
import Graphics.Gloss.Interface.Pure.Animate (animate)
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Raster.Field (animateField, rgbI)

test :: IO ()
test = do
  animateField
    (InWindow "fractal" windowSize (50, 10))
    (pixelSize, pixelSize)
    frame
  where
    windowSize = (1333, 1000)
    pixelSize = 1

type Time = Float

{-# INLINE cycleColorF #-}
cycleColorF :: Float -> Int
cycleColorF fc = round fc  `mod` 256

{-# INLINE cycleColor #-}
cycleColor :: Int -> Int
cycleColor = (`mod` 256)

-- colorTable
colorTable =
  [ rgbI  66  30  15
  , rgbI  25   7  26
  , rgbI   9   1  47
  , rgbI   4   4  73
  , rgbI   0   7 100
  , rgbI  12  44 138
  , rgbI  24  82 177
  , rgbI  57 125 209
  , rgbI 134 181 229
  , rgbI 211 236 248
  , rgbI 241 233 191
  , rgbI 248 201  95
  , rgbI 255 170   0
  , rgbI 204 128   0
  , rgbI 153  87   0
  , rgbI 106  52   3
  ]

field :: Time -> Point -> Int
field time (x, y) | endVal < limit  = length allVals
                  | otherwise       = 0
  where endVal   = magnitude (last allVals)
        allVals  = q (x:+y)

type C = Complex Float

limit = 4

maxIter = 256

q :: C -> [C]
q p = take maxIter (takeWhile ((<limit).magnitude) (iterate (f p) 0))

f :: C -> (C -> C)
f a z = z^2 + a


{-# INLINE frame #-}
frame :: Time -> Point -> Color
frame time p | v == maxIter  = rgbI 0 0 0
             | otherwise     = colorTable !! (v `mod` 16)
  where
    v = field time (rescale startRect p)


type Rect = (Point,Point) -- centre, scale

startRect :: Rect
startRect = cornersToCentreScale ( (-2.2,-1.2), (1,1.2) )

cornersToCentreScale (lowLeft, upRight) = (centre, scale)
  where
    centre = (upRight .+ lowLeft) ./ (2,2)
    scale  = (upRight .- lowLeft) ./ (2,2)

-- rescale maps the gloss coordinates in ((-1,-1)-(1,1)) to a rectangle around centre
-- rescale :: Rect -> Point -> Point
rescale (centre, scale) p = centre .+ (scale .* p)


(.+), (.-), (.*), (./) :: Point -> Point -> Point
(x1, y1) .+ (x2, y2) = (x1+x2, y1+y2)
(x1, y1) .- (x2, y2) = (x1-x2, y1-y2)
(x1, y1) .* (x2, y2) = (x1*x2, y1*y2)
(x1, y1) ./ (x2, y2) = (x1/x2, y1/y2)


-- http://abacus.bates.edu/~sross/bifurandorbit01.html

----------------------------------------------------------------
sigma = 1
sigma2 = sigma^2
maxCol' :: Float
maxCol' = 500
{-# INLINE maxCol #-}
maxCol :: Float -> Float
maxCol t = maxCol' * (sin t)^2
{-# INLINE field #-}
field' :: Time -> Point -> Float
field' time (x, y) = (maxCol time / sqrt (2*pi*sigma)) * exp (-(r2/sigma2))
  where r2 = x^2 + y^2


-- older testing stuff

run fileName = do
  picture@(Bitmap width height _ _)
    <- loadBMP fileName
  let offset = (10,  10)
  display
    (InWindow fileName (width, height) offset)
    black $ Pictures
      [ rectangleSolid (fromIntegral width) (fromIntegral height)
      , picture]

-- Perhaps later: check out
--    https://hackage.haskell.org/package/gloss-raster-1.10.2.4/docs/Graphics-Gloss-Raster-Array.html
-- or https://hackage.haskell.org/package/gloss-raster-accelerate-1.9.0.0/docs/src/Graphics-Gloss-Accelerate-Raster-Array.html

-- Look at simulate or play
--   https://hackage.haskell.org/package/gloss-1.10.2.3/docs/Graphics-Gloss.html#v:simulte

test2 :: IO ()
test2 = run "../img/PaJa_tiny.bmp"
