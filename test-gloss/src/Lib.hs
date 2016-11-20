module Lib
    ( test
    ) where
import Graphics.Gloss
import System.Environment

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

test :: IO ()
test = run "../img/PaJa_tiny.bmp"
