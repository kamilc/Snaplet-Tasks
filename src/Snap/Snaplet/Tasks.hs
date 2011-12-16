{-# LANGUAGE OverloadedStrings #-}
-- | This module contains definition of a Tasks Snaplet for 
--   Snap >= 0.7.*. It allows Snap app developers to create command 
--   line tasks akin to "rake tasks" found in Ruby On Rails framework.
--   
--   Essenstially, this snaplet let's your other snaplet's to have 
--   their administrative tasks that You'd call from command line 
--   to - let's say create indexes in Your DB or screen scrape some 
--   useful data from some service and save it in DB.
--
--   Every task lives in MonadSnap defined by some Snaplet.
--   That means, that if your web app defines some task, it'll be
--   executed in MonadSnap as if it were a Handler for some route.
module Snap.Snaplet.Tasks( tasksInit,
                           TasksSnaplet,
                           module Snap.Snaplet.Tasks.Utils ) where

import           Snap.Snaplet
import           Snap.Snaplet.Tasks.Internal
import           Snap.Snaplet.Tasks.Utils

import           Control.Monad.Reader
import           Control.Concurrent

data TasksSnaplet = TasksSnaplet

-- | This method spawns a new thread that waits till
--   server is ready and then fires given task.
tasksInit :: SnapletInit b TasksSnaplet
tasksInit = makeSnaplet "tasks" "Snap Tasks" Nothing $ do
  liftIO $ do
    mtask <- taskFromCommandLine
    case mtask of
      Nothing   -> return ()
      Just task -> (forkIO $ runTask task) >> return ()
  return $ TasksSnaplet