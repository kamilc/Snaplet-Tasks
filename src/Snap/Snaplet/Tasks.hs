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
--   Every task is in fact just a handler for route. Those routes 
--   are hashes for routes so that 'somens:other:cool:task' becomes
--   a valid route in app.
--   To create such task in one of Your snaplets (maybe in your app
--   snaplet) - define route for it using handy 'task' function
--   that this module reexports.
--
--   Running tasks is fairly simple: 
--   yourapp T snaplet:super:cool arg1=v1 arg2=v2 [-p 1000]
--   This means that your task command always follows 'T'
--   If you're running Your app at default port You can specify
--   different port at the end by using standard -p argument.

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