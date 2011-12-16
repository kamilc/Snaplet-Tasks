module Paths_snaplet_tasks (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/snaplet-tasks-0.1/ghc-7.0.3"
datadir    = "/usr/local/share/snaplet-tasks-0.1"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "snaplet_tasks_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "snaplet_tasks_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "snaplet_tasks_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "snaplet_tasks_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
