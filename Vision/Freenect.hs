module Vision.Freenect
  ( initialize
  , withKinect
  ) where

import Control.Monad.Fix
import Vision.Freenect.Convert
import Freenect
import Data.IORef

-- | @withKinect (\kill img -> f kill img)@ will call @f@ continually
-- until @f@ calls @term@.  It is possible @f@ will be called after calling
-- @kill@, but the callbacks should stop \"soon\".
withKinect :: (IO () -> Depth -> IO ()) -> IO ()
withKinect callback = do
    continueRef <- newIORef True
    let terminate = writeIORef continueRef False
    withContext $ \ctx -> do
       selectSubdevices ctx [Camera]
       withDevice ctx 0 $ \dev -> do
        setDepthMode dev Medium ElevenBit
        let myCallback v _ = callback terminate (depthToFriday v)
        setDepthCallback dev myCallback
        startDepth dev
        fix $ \cont -> do
                 c <- readIORef continueRef
                 if c then processEvents ctx >> cont
                      else return ()
