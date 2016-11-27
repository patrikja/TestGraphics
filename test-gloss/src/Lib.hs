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
    (InWindow "foo" windowSize (50, 10))
    (pixelSize, pixelSize)
    frame
  where
    windowSize = (1024, 768)
    pixelSize = 1

type Time = Float

{-# INLINE cycleColor #-}
cycleColor :: Float -> Int
cycleColor fc = round fc  `mod` 256

field :: Time -> Point -> Float
field time (x, y) = magnitude (last (q (x:+y)))

type C = Complex Float

q :: C -> [C]
q p = take 30 (takeWhile ((<255).magnitude) (iterate (f p) 0))

f :: C -> (C -> C)
f a z = z^2 + a


{-# INLINE frame #-}
frame :: Time -> Point -> Color
frame time p = rgbI r g b
  where
    v = field time p
    r = cycleColor v
    g = cycleColor v
    b = cycleColor v

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
