module Vision.Freenect.Convert where

import Vision.Image as F
import Vision.Primitive
import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)
import Data.Word

type Depth = Manifest Word16

-- | Convert 'Depth' images into a gray scale image.
depthToGrey :: Depth -> Grey
depthToGrey = F.map (\x -> floor $ 255 * (fromIntegral x / 4096 :: Double))

-- | Convert depth information into an 2D image of 'Word16' values.
depthToFriday   :: Vector Word16 -> Depth
depthToFriday v = fromFunction sp (\(Z :. col :. row) -> fromIntegral $ v V.! (col * width + row))
  where
  sp@(Z :. _height :. width) =
      case V.length v of
            307200 -> Z :. 480 :. 640
            _      -> error "resolution is unsupported"

rgbToFriday :: Vector Word8 -> RGB
rgbToFriday v = fromFunction sp (\p -> RGBPixel (f p 0) (f p 1) (f p 2))
 where
  f (Z :. y :. x) chan = v V.! ((y*width + x)*3 + chan)
  sp@(Z :. _height :. width) =
      case V.length v of
            307200 -> Z :. 480 :. 640
            _      -> error "resolution is unsupported"
