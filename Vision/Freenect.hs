{-# LANGUAGE RecordWildCards #-}

module Vision.Freenect
  ( initialize
  , withKinect, withKinect'
  -- * Types
  , Depth
  , RunStatus(..), Config(..)
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad.Fix
import Vision.Freenect.Convert
import Freenect
import Data.IORef

-- | A pseudo-boolean indicating whether the process should continue or halt.
data RunStatus = Stop | Continue deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Structure for configuring the depth callback, method to acquire the
-- run status (stopping the callbacks), and amount of delay between frame
-- acquisition (in micro seconds, 100000 ~ 10fps).
data Config = Config { depthCallback :: Depth -> IO ()
                     , killSignal    :: IO RunStatus
                     , delayTime     :: Int
                     }

-- | @withKinect (\kill img -> f kill img)@ will call @f@ continually
-- until @f@ calls @kill@.  It is possible @f@ will be called after calling
-- @kill@, but the callbacks should stop \"soon\".  There should be at most
-- 10 callbacks per second (@delayTime=100000@).
withKinect :: (IO () -> Depth -> IO ()) -> IO ()
withKinect cb =
  do continueRef <- newIORef Continue
     let kill    = writeIORef continueRef Stop
         cb'     = cb kill
         killSig = readIORef continueRef
     withKinect' Config { depthCallback = cb'
                        , killSignal    = killSig
                        , delayTime     = 100000 -- max of 10fps
                        }

withKinect' :: Config -> IO ()
withKinect' (Config {..}) = do
    withContext $ \ctx -> do
       selectSubdevices ctx [Camera]
       withDevice ctx 0 $ \dev -> do
        setDepthMode dev Medium ElevenBit
        let myCallback v _ = depthCallback (depthToFriday v)
        setDepthCallback dev myCallback
        startDepth dev
        fix $ \cont -> do
                 c <- killSignal
                 if c == Continue
                  then processEvents ctx >> threadDelay delayTime >> cont
                  else return ()
