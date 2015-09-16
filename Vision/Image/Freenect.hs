module Vision.Image.Freenect where

import Vision.Image
import Vision.Primitive
import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)
import Data.Word

-- | Convert depth information into an 8-bit gray image
depthToFriday   :: Vector Word16 -> Grey
depthToFriday v = fromFunction shape (\(Z :. col :. row) -> fromIntegral $ v V.! (col + row * width))
  where
  shape@(Z :. height :. width) =
      case V.length v of
            307200 -> Z :. 480 :. 640
            _      -> error "resolution is unsupported"

rgbToFriday :: Vector Word8 -> RGB
rgbToFriday v = fromFunction shape (\p -> RGBPixel (f p 0) (f p 1) (f p 2))
 where
  f (Z :. y :. x) chan = v V.! ((y*width + x)*3 + chan)
  shape@(Z :. height :. width) =
      case V.length v of
            307200 -> Z :. 480 :. 640
            _      -> error "resolution is unsupported"
