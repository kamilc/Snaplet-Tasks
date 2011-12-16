module Snap.Snaplet.Tasks.Utils(task) where

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Tasks.Internal

import qualified Data.ByteString.Char8 as B
import qualified Data.List.Utils as U
import           Data.List (intercalate)

-- | Helper function for creating tasks in Snaplets routes
--   that are available through Tasks Snaplet interface
task :: String -> (Handler b v ()) -> (B.ByteString, Handler b v ())
task name handler = (B.pack $ hashString canonizedName, taskify handler)
  where
    canonizedName = intercalate "/" $ U.split ":" name

taskify :: Handler b v () -> Handler b v ()
taskify handler = do
   --now here we *must* check if our apps IP
   --is the same as request's IP and if it is
   --continue or else pass
  setTimeout $ 60*60*24*31 -- approx 31 days
  localIp   <- return . rqLocalAddr  =<< getRequest
  requestIp <- return . rqRemoteAddr =<< getRequest
  case localIp == requestIp of
    True  -> handler
    False -> pass